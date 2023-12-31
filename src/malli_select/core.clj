(ns malli-select.core
  "Select a subset of a malli schema."
  (:require [clojure.pprint :refer [pprint]]
            [malli.core :as m]
            [malli.util :as mu]))

(defn- clean-path [path]
  (loop [p path
         r (transient [])]
    (if-not (seq p)
      (persistent! r)
      (recur (rest p) (let [phead (first p)]
                        (if (#{0 1 :malli.core/in} phead)
                          r
                          (conj! r phead)))))))


(defn- map-schema-path-walker [f]
  (fn [schema path children _]
    (let [schema      (m/-set-children schema children)
          map-schema? (= :map (m/type schema))]
      (cond-> schema
        map-schema? (f path)))))


(defn- sel->map
  "Turns `[:a {:b [:c]} {:b [:d]} :e]`
  into `{nil [:a :e] :b [:d]}` (i.e. last `:b` wins)."
  [sel]
  (persistent! (reduce (fn [acc i]
                         (if (map? i)
                           (reduce conj! acc i)
                           (assoc! acc nil (conj (get acc nil) i)))) (transient {}) sel)))


(defn- parse-selection
  "Examples:
  ```
  (parse-selection []) ;; => [['?]]
  (parse-selection [:name]) ;; => [[:name]]
  (parse-selection [:name {:address [:street]}]) ;; => [[:name] [:address :street]]
  ```
  "
  ([sel] (parse-selection sel []))
  ([sel path]
   (if-not (seq sel)
     [(conj path '?)]
     (let [sel-map (sel->map sel)]
       (persistent!
        (reduce-kv (fn [acc k v]
                     (if (nil? k)
                       (reduce conj! acc (map #(conj path %) v))
                       (reduce conj! acc (parse-selection v (conj path k))))) (transient []) sel-map))))))


(defn- paths->tree [paths]
  (reduce (fn [acc path]
            (let [[folder name] ((juxt pop peek) path)]
              (update acc folder (fnil conj #{}) name))) {} paths))


(defn selectable-paths
  "Yield set of selectable paths.

  Examples:
  ```
  (selectable-paths
    [:maybe
      [:map
        [:addresses [:vector [:map
                               [:street string?]]]]]])
  ;;=> #{[:addresses] [:addresses :street]}
  ```
  "
  [schema]
  (->> schema
       mu/subschemas
       (map (comp clean-path :path))
       (filter seq)
       set))


(defn- -select
  [schema selection
   {:as   _options
    ::keys [optionalized]
    :keys [verify-selection prune-optionals]
    :or
    {verify-selection :assert}}]
  (letfn [(in? [coll elm]
            (some #(= % elm) coll))]
    (let [all-optional?            (empty? selection)
          verify-selection?        (and (not (in? #{nil false :skip} verify-selection))
                                        (not all-optional?))
          prune-optionals          (if (not (nil? prune-optionals))
                                     prune-optionals
                                     (-> selection meta :only))
          selection-paths          (parse-selection selection)
          sel-map                  (paths->tree selection-paths)
          !available-paths         (atom #{})
          !seen                    (atom #{})
          record-seen!             (fn [schema path to-require]
                                     (when verify-selection?
                                       (let [available-keys (map first (m/entries schema))
                                             valid-keys     (into ['? '*] available-keys)
                                             seen-keys      (filter to-require valid-keys)]
                                         (swap! !available-paths into
                                                (map (partial conj path) available-keys))
                                         (swap! !seen into
                                                (map (partial conj path) seen-keys)))))
          !prune-exclusions        (atom #{})
          record-prune-exclusions! (fn [path]
                                     (when prune-optionals
                                       (let [self&parent-paths (take (inc (count path)) (iterate pop path))]
                                         (swap! !prune-exclusions into self&parent-paths))))

          walker (let [optionalize-step (fn optionalize-step [v]
                                          (update v 0 mu/optional-keys))
                       require-step     (fn require-step [[schema path :as v]]
                                          (let [cleaned-path (clean-path path)
                                                to-require   (sel-map cleaned-path)]
                                            (if-not (seq to-require)
                                              v
                                              (let [star? (some #{'*} to-require)]
                                                (record-seen! schema cleaned-path to-require)
                                                (record-prune-exclusions! path)
                                                (update v 0
                                                        #(if star?
                                                           (mu/required-keys %)
                                                           (mu/required-keys % to-require)))))))

                       prune-step  (fn prune-step [[schema path :as v]]
                                     (let [prunable? (every-pred (comp :optional second)
                                                                 (comp not @!prune-exclusions #(conj path %) first))
                                           children  (remove prunable? (m/children schema))]
                                       (update v 0 #(m/into-schema (m/type %) (m/-properties %)
                                                                   children (m/-options %)))))
                       wrap        (fn [stack step]
                                     #(step (stack %)))
                       middlewares (cond-> identity
                                     (not optionalized)  (wrap optionalize-step)
                                     (not all-optional?) (wrap require-step)
                                     prune-optionals     (wrap prune-step)
                                     :finally            (wrap first))]
                   (map-schema-path-walker (comp middlewares vector)))
          walked (m/walk schema walker
                         {::m/walk-schema-refs true ::m/walk-refs true})]
      (when verify-selection?
        (let [invalid-selection-paths (remove @!seen selection-paths)]
          (assert (empty? invalid-selection-paths)
                  (str "Selection contains unknown paths: " (prn-str invalid-selection-paths)
                       "\nAvailable: \n" (with-out-str (pprint (sort (selectable-paths schema))))))))

      walked)))


(defn select
  "`selection` examples:
  - `[]` - everything (deep) optional
  - `[:name :age]` - required attributes
  - `['*]` - everything (non-recursive) required
  - `[{:address [:street]}]` - if `:address` provided then only `:street` is required.

  Combinations:
  - `[:address {:address [:street]}]` - require `:address` but only its `:street` is required.
  - `[:address {:address [] :friends [:name]}]` - require `:address` and optionally `:friends`.
  - `[{:friends [:name]} {:friends [:age]}]` - only require `:age` of friends if `:friends` provided (last selection wins).

  `options`:
  - `verify-selection` (`:assert` (default), `:skip`, `false`, `nil`) - what to do when `selection` contains paths not in `schema`.
  - `prune-optionals` (`false` (default), `true`) - whether all fully optional subtrees should be removed from the resulting schema. Alternatively via metadata of selection: `^:only [:name]` (flag takes precedence over metadata).
    Typically used when the selected schema is used for data generation.

  Examples:
  ```
  (select Person)                   ;; all optional
  (select Person [])                ;; all optional
  (select Person ['*])              ;; all root attributes of Person required
  (select Person [:name :handle])   ;; Require specific root attributes.
  (select Person [{:address ['*]}]) ;; Require the full address if provided.

  (select Person [:foo]) ;; Assert exception about non existing path, showing all possible paths.
  ```
  "
  ([schema]
   (select schema [] nil))
  ([schema selection]
   (select schema selection nil))
  ([schema selection {:as   options
                      :keys [verify-selection prune-optionals]
                      :or   {verify-selection :assert}}]
   (-select schema selection (assoc options
                                    :verify-selection verify-selection
                                    :prune-optionals prune-optionals))))


(defn selector
  "Yields a function similar to `(partial ms/select schema)`.
  A selector is faster when doing multiple selections from a schema as the schema is optionalized once.

  Examples:
  ```
  (let [person-selector (selector Person)]
    (person-selector ^:only [:name]))
  ```
  "
  [schema]
  (let [optionalized-schema (select schema)]
    (fn selector-select
      ([selection] (selector-select selection nil))
      ([selection options]
       (-select optionalized-schema selection (merge {::optionalized true} options))))))

(comment
  (def Person
    [:map
     [:name string?]
     [:age int?]
     [:friends [:vector [:map [:name string?]]]]
     [:addresses [:vector [:map
                           [:street string?]
                           [:country string?]]]]])

  (let [person-selector (selector Person)]
    (m/form (person-selector [:name] {:prune-optionals true})))

  (let [schema [:schema {:registry {"More"  [:map
                                             [:more boolean?]]
                                    "Other" [:map
                                             [:other boolean?]
                                             [:more "More"]]}}
                [:map
                 [:this boolean?]
                 [:that "Other"]]]
        selector (selector schema)]
    (cc/quick-bench (selector ^:only [:this {:that ['*]}]))
    #_(cc/quick-bench (selector schema)))

  (require '[criterium.core :as cc])


  (cc/quick-bench (select Person ^:only [:name]))

  (def person-selector (selector Person))
  (cc/quick-bench (selector Person)) ;; 10us
  (cc/quick-bench (select Person ^:only [:name {:addresses [:street]}])) ;; 23us
  (cc/quick-bench (person-selector ^:only [:name {:addresses [:street]}])) ;; 13us

  (m/form (select [:maybe Person] ^:only [:name {:friends [:name]}]))


  #_:end)

(ns dk.thinkcreate.malli-select
  "Select a subset of a malli schema."
  (:require [malli.core :as m]
            [malli.util :as mu]))

(defn- star-or-question-selection? [sel]
  (-> sel peek #{'? '*}))


(defn- question-selection? [sel]
  (-> sel peek (= '?)))


(defn- star-selection? [sel]
  (-> sel peek (= '*)))

;; ~4,8us
(defn- requirable-paths [schema]
  (let [options {::m/walk-schema-refs true ::m/walk-refs true}
        state   (atom [])]
    (mu/find-first schema
                   (fn [_ p _] (when-not (#{0 1 :malli.core/in} (peek p)) (swap! state conj p)) nil) options)
    @state))


(defn- clean-path [path]
  (loop [p path
         r (transient [])]
    (if-not (seq p)
      (persistent! r)
      (recur (rest p) (let [phead (first p)]
                        (if (#{0 1 :malli.core/in} phead)
                          r
                          (conj! r phead)))))))


(defn- path-matches-selection?
  "Examples:
  (path-matches-selection? [:foo] [:foo]) ;; => true
  (path-matches-selection? [:foo :bar] [:foo '?]) ;; => true
  (path-matches-selection? [:foo :bar] [:foo '*]) ;; => true
  (path-matches-selection? [:foo :bar :baz] [:foo '*]) ;; => false"
  [path sel]
  (or (and (star-or-question-selection? sel)
        (= (count path) (count sel))
        (= (pop path) (pop sel)))
      (= path sel)))


(defn- schema-path-walker [f]
  (fn [schema path children _]
    (f (m/-set-children schema children) path)))


;; `selector` will receive `path` (of the map).
(defn- deep-mark-paths
  ([schema selector] (deep-mark-paths schema selector nil))
  ([schema selector {:keys [all-optional?]}]
   (m/walk schema
           (schema-path-walker (fn [s path]
                                 (if (= :map (m/type s))
                                   (let [required-keys (selector path)]
                                     (cond
                                       all-optional?       (mu/optional-keys s)
                                       (seq required-keys) (mu/required-keys (mu/optional-keys s) required-keys)
                                       :else               (mu/optional-keys s)))
                                   s)))
           {::m/walk-schema-refs true ::m/walk-refs true})))


(defn deep-prune-optionals [schema]
  (let [non-prunable-paths   (atom #{})
        prunable-child-path? (fn [path [name & _ :as _child]]
                               (not (@non-prunable-paths (conj path name))))]
    (m/walk schema
            (fn [s path children _options]
              #_(prn :s s :path path :childre children)
              (let [map-schema?     (= :map (m/type s))
                    prunable-child? (every-pred (comp :optional second)
                                                (partial prunable-child-path? path))
                    children        (if map-schema? (remove prunable-child? children) children)]
                (when (and map-schema? (seq children))
                  (let [parent-paths (take (inc (count path)) (iterate pop path))]
                    (swap! non-prunable-paths into parent-paths)))
                (m/into-schema (m/type s) (m/-properties s) children (m/-options s))))
            {::m/walk-schema-refs true ::m/walk-refs true})))


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
  (let [f (fn [path]
            (for [i (range (count path))]
              [(subvec path 0 i) (nth path i)]))]
    (persistent! (reduce (fn [acc [a b]]
                           (assoc! acc a (conj (get acc a #{}) b)))
                         (transient {}) (mapcat f paths)))))


(defn- schema-paths->mapper [paths]
  (reduce (fn [acc sel-path]
            (update acc (pop sel-path) (fnil conj '()) (peek sel-path)))
          {} paths))


(defn- remove-fns-for-selection-dispatch [sel]
  (cond
    (question-selection? sel) :question-selection
    :else                     :default))

(defmulti #^{:private true} remove-fns-for-selection #'remove-fns-for-selection-dispatch)

(defmethod remove-fns-for-selection :question-selection [sel]
  #(path-matches-selection? % sel)
  #_#(shorter-subpath? % (pop sel)))

(defmethod remove-fns-for-selection :default [_sel]
  (constantly false))


(defn- filter-fns-for-selection-dispatch [sel]
  (cond
    (question-selection? sel) :question-selection
    (star-selection? sel)     :star-selection
    :else                     :simple-selection))


(defmulti #^{:private true} filter-fns-for-selection #'filter-fns-for-selection-dispatch)

(defmethod filter-fns-for-selection :simple-selection [sel]
  #(= % sel))

;; [:address '*]
;; [:address :country] [:address]
(defmethod filter-fns-for-selection :star-selection [sel]
  #(path-matches-selection? % sel))


(defmethod filter-fns-for-selection :question-selection [_sel]
  (constantly false))


(defn- required-schema-paths [cleaned-path-to-schema-path-mapping selection-paths]
  (let [remove-fns (map remove-fns-for-selection selection-paths)
        filter-fns (map filter-fns-for-selection selection-paths)]
    (sequence
     (comp (remove (apply some-fn remove-fns))
           (filter (apply some-fn filter-fns))
           (map cleaned-path-to-schema-path-mapping)) (keys cleaned-path-to-schema-path-mapping))))


(defn- invalid-paths [cleaned-schema-paths selection-paths]
  (remove #(some (fn [cp] (path-matches-selection? cp %))
                 cleaned-schema-paths) selection-paths))


(defn select
  "`selection` examples:
  `[]` - everything (deep) optional
  `[:name :age]` - required attributes
  `['*]` - everything (non-recursive) required
  `[{:address [:street]}]` - if `:address` provided then only `:street` is required.

  Combinations:
  `[:address {:address [:street]}]` - require `:address` but only its `:street` is required.
  `[:address {:address [] :friends [:name]}]` - require `:address` and optionally `:friends`.
  `[{:friends [:name]} {:friends [:age]}]` - only require `:age` of friends if `:friends` provided (last selection wins).

  `options`:
  - `verify-selection` (`:assert` (default), `:skip`, `false`, `nil`) - what to do when `selection` contains paths not in `schema`.
  - `prune-optionals` (`false` (default), `true`) - whether all fully optional subtrees should be removed from the resulting schema.
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
  ([schema] (select schema [] nil))
  ([schema selection] (select schema selection nil))
  ([schema selection
    {:as   _options
     :keys [verify-selection prune-optionals]
     :or
     {verify-selection :assert
      prune-optionals  false}}]
   (letfn [(in? [coll elm]
             (some #(= % elm) coll))]
     (let [prune-optionals                     (or (true? prune-optionals) (-> selection meta :only))
           schema                              (m/schema schema)
           all-optional?                       (= [] selection)
           selection-paths                     (parse-selection selection)
           verify-selection?                   (and (not (in? #{nil false :skip} verify-selection))
                                                    (not all-optional?))
           cleaned-path-to-schema-path-mapping (as-> (requirable-paths schema) $ ;; {[:street '?] [0]}
                                                 (zipmap (map clean-path $) $))
           unknown-selections                  (when verify-selection?
                                                 (invalid-paths (keys cleaned-path-to-schema-path-mapping)
                                                                selection-paths))
           required-spaths                     (when-not all-optional?
                                                 (required-schema-paths
                                                  cleaned-path-to-schema-path-mapping
                                                  selection-paths))
           required-paths-mapper               (schema-paths->mapper required-spaths)]
       (assert (empty? unknown-selections)
               (str "Selection contains unknown paths: " (prn-str unknown-selections)
                    "\nAvailable: " (prn-str (sort (keys cleaned-path-to-schema-path-mapping)))))
       (cond-> schema
         :always         (deep-mark-paths required-paths-mapper
                                          {:all-optional? all-optional?})
         prune-optionals deep-prune-optionals)))))


(comment

  (parse-selection [{:address [:street]} {:address [{:country [:iso]}]}])

  (requirable-paths [:schema {:registry {"Other" [:map
                                                  [:other boolean?]]}}
                     [:map
                      [:this boolean?]
                      [:that "Other"]]])
  (require '[criterium.core :as cc]
           '[malli.generator :as mg])

  (def Person
    [:map
     [:name string?]
     [:age [:int {:min 0 :max 10}]]
     [:friends [:vector [:map [:name string?]]]]
     [:address [:map
                [:street string?]
                [:number int?]
                [:country [:map
                           [:iso string?]
                           [:name string?]]]]]])

  (cc/quick-bench (select Person [:address {:address ['*]}]))

  (paths->tree (parse-selection [{:address ['*]}])) [:address '*]

  (m/validate (select Person ['*]) {:name "Gert" :age 1 :friends [] :address {}})

  (m/validate (select Person ['* #_{:address [:country {:country ['*]}]}])
              {:name "Gert" :age 1 :friends [] :address {}})

  (parse-selection [:foo {:bar [] :baz []}])

  (requirable-paths [:map-of string? [:vector Person]])
  (parse-selection [:name :age {:address [:street]}])
  (m/validate (select Person [:address2]) {:address {:street ""}})

  (m/validate Person {:name "" :age 1 :address nil})

  (mg/generate Person)
  (def s1 [:name])
  (def s2 [{:address [:street]} :age])
  (def s3 [:name {:address ['*]} :age])
  (def s4 [:address {:address []}])
  (def s5 [{:address [:street {:country [:name '*]}]} :name])
  (def s6 [{:address [:street {:country [:name '* :iso]} :number '*]} :name :age '*])

  (def Course [:map
               [:hash [:string {:min 6 :max 6}]]
               [:title string?]
               [:subtitle string?]
               [:audio [:map
                        [:length pos-int?]]]
               [:image [:map
                        [:thumbnails [:vector [:map [:thumb-x-1 string?]]]]]]])

  (m/walk Course (m/schema-walker (fn [s])))
  (m/form (select
           Course [{:audio [:length]}]))

  (m/entries Course)
  (m/form (select Person [:name]))

  (require '[clj-async-profiler.core :as prof])
  (prof/profile (dotimes [i 10000] (select Person s3)))

  (prof/serve-files 8081))

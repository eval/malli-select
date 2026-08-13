(ns malli-select.core
  "Select a subset of a malli schema."
  (:require [clojure.set :as set]
            [malli.core :as m]
            [malli.util :as mu]))

(def ^:dynamic *verify-selection*
  "Default for the `:verify-selection` option of `select`/`selector`.
  See `select` for accepted values.

  Rebind with `binding`, or set app-wide via `alter-var-root`
  (Clojure) / `set!` (ClojureScript)."
  :throw)

(defn- -fail! [type data]
  (throw (ex-info (str type " " (pr-str data)) {:type type :data data})))

(defn- -warn! [type data]
  (let [msg (str type " " (pr-str data))]
    #?(:clj  (binding [*out* *err*] (println "WARNING:" msg))
       :cljs (js/console.warn msg))))

(defn- clean-path [path]
  (loop [p path
         r (transient [])]
    (if-not (seq p)
      (persistent! r)
      (recur (rest p) (let [phead (first p)]
                        (if (#{0 1 :malli.core/in} phead)
                          r
                          (conj! r phead)))))))

(defn- branch-seg?
  "Is `x` a path segment addressing a branch of a `:multi`?"
  [x]
  (and (map? x) (contains? x :branch)))

(defn- untag-path
  "Strip `{:branch k}` tags from `path`."
  [path]
  (mapv #(if (branch-seg? %) (:branch %) %) path))

(defn- tag-path
  "Tag every segment of cleaned `path` that crosses a `:multi` (per
  `multi-paths`) as `{:branch seg}`."
  [multi-paths path]
  (if (empty? multi-paths)
    path
    (loop [prefix [] tagged [] [seg & more :as segs] (seq path)]
      (if-not segs
        tagged
        (recur (conj prefix seg)
               (conj tagged (if (contains? multi-paths prefix) {:branch seg} seg))
               more)))))

(defn- multi-branch-keys
  "Map of cleaned path → set of branch keys, for every `:multi` in `schema`."
  [schema]
  (let [!acc (atom {})]
    (m/walk schema
            (fn [s path _ _]
              (when (= :multi (m/type s))
                (swap! !acc assoc (clean-path path) (into #{} (map first) (m/children s))))
              s)
            {::m/walk-schema-refs true ::m/walk-refs true})
    @!acc))

(defn- multi-dispatch-paths
  "Map of raw path → dispatch key, for every `:multi` in `schema` with a
  keyword `:dispatch`."
  [schema]
  (let [!acc (atom {})]
    (m/walk schema
            (fn [s path _ _]
              (when (= :multi (m/type s))
                (let [dispatch (:dispatch (m/properties s))]
                  (when (keyword? dispatch)
                    (swap! !acc assoc path dispatch))))
              s)
            {::m/walk-schema-refs true ::m/walk-refs true})
    @!acc))

(defn- schema-path-walker
  "Walker that fires `map-f` at `:map` nodes and `multi-f` at `:multi` nodes.
  `multi-f` also receives the original (pre-walk) schema."
  [map-f multi-f]
  (fn [schema path children _]
    (let [walked (m/-set-children schema children)]
      (case (m/type schema)
        :map   (map-f walked path)
        :multi (multi-f walked schema path)
        walked))))


(defn- merge-selections
  "Merge two sub-selections for the same key: vectors concatenate, branch-maps
  merge per branch. On a shape mismatch the last one wins (verification then
  reports the wrong-shaped one)."
  [a b]
  (cond
    (and (map? a) (map? b))               (merge-with merge-selections a b)
    (and (sequential? a) (sequential? b)) (into (vec a) b)
    :else                                 b))

(defn- sel->map
  "Turns `[:a {:b [:c]} {:b [:d]} :e]`
  into `{nil [:a :e] :b [:c :d]}` (i.e. duplicate keys merge)."
  [sel]
  (persistent! (reduce (fn [acc i]
                         (if (map? i)
                           (reduce-kv (fn [acc k v]
                                        (assoc! acc k (merge-selections (get acc k []) v))) acc i)
                           (assoc! acc nil (conj (get acc nil) i)))) (transient {}) sel)))


(defn- parse-selection
  "Examples:
  ```
  (parse-selection []) ;; => [['?]]
  (parse-selection [:name]) ;; => [[:name]]
  (parse-selection [:name {:address [:street]}]) ;; => [[:name] [:address :street]]
  (parse-selection {:human [:name]}) ;; => [[{:branch :human} :name]]
  ```
  A selection that *is* a map addresses branches of a `:multi`: keys are
  dispatch values (`'*` for every branch), values are sub-selections."
  ([sel] (parse-selection sel []))
  ([sel path]
   (cond
     (not (seq sel))
     [(conj path '?)]

     (map? sel)
     (persistent!
      (reduce-kv (fn [acc k v]
                   (reduce conj! acc (parse-selection v (conj path {:branch k}))))
                 (transient []) sel))

     :else
     (let [sel-map (sel->map sel)]
       (persistent!
        (reduce-kv (fn [acc k v]
                     (if (nil? k)
                       (reduce conj! acc (map #(conj path %) v))
                       (reduce conj! acc (parse-selection v (conj path k))))) (transient []) sel-map))))))
(comment
  (sel->map {'* []})
  (parse-selection {'* []})
  (select [:vector [:multi {:dispatch :type} [:foo [:map [:f :string]]] [::m/default [:map [:d :string]]]]] {::m/default [:d]})
  #_:end)

(defn- expand-star-branches
  "Expand every `{:branch '*}` segment of `path` into one path per branch of
  the `:multi` at that position. Positions without a known `:multi` keep the
  star segment (so verification reports it)."
  [multi-paths path]
  (loop [expanded [[]] [seg & more :as segs] (seq path)]
    (if-not segs
      expanded
      (recur (if (= {:branch '*} seg)
               (vec (mapcat (fn [p]
                              (if-let [ks (seq (get multi-paths (untag-path p)))]
                                (map #(conj p {:branch %}) ks)
                                [(conj p seg)]))
                            expanded))
               (mapv #(conj % seg) expanded))
             more))))


(defn- paths->tree [paths]
  (reduce (fn [acc path]
            (let [[folder name] ((juxt pop peek) path)]
              (update acc folder (fnil conj #{}) name))) {} paths))


(defn selectable-paths
  "Yield set of selectable paths. Branches of a `:multi` show up as
  `{:branch dispatch-value}` segments.

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
  (let [subs   (mu/subschemas schema)
        multis (into {}
                     (keep (fn [{s :schema p :path}]
                             (when (= :multi (m/type s))
                               [(clean-path p) (into #{} (map first) (m/children s))])))
                     subs)]
    (->> subs
         (map (comp #(tag-path multis %) clean-path :path))
         (filter seq)
         set)))


(defn- star-available
  "For every failing `'*`-branch selection in `origin-paths`: the attribute
  paths available in *all* branches of the `:multi` at that position, spelled
  as `{:branch '*}` paths."
  [multi-paths available origin-paths]
  (for [p     origin-paths
        :let  [idx (first (keep-indexed (fn [i seg] (when (= {:branch '*} seg) i)) p))]
        :when idx
        :let  [prefix     (subvec (vec p) 0 idx)
               branch-ks  (get multi-paths (untag-path prefix))
               per-branch (map (fn [k]
                                 (let [bp (conj prefix {:branch k})]
                                   (into #{} (keep #(when (and (seq %) (= bp (pop %))) (peek %))) available)))
                               branch-ks)
               common     (if (seq per-branch) (apply set/intersection per-branch) #{})]
        attr  common]
    (conj prefix {:branch '*} attr)))

(defn- shape-mismatch?
  "True when `path` addresses a `:multi` position without a `{:branch _}`
  segment, or a non-multi position with one."
  [multi-paths path]
  (boolean
   (loop [prefix [] [seg & more :as segs] (seq path)]
     (when segs
       (if (= (contains? multi-paths (untag-path prefix)) (branch-seg? seg))
         (recur (conj prefix seg) more)
         true)))))


(defn- -select
  [schema selection
   {:as   options
    ::keys [optionalized keep-only]
    :keys [verify-selection prune-optionals]
    :or
    {verify-selection *verify-selection*}}]
  (letfn [(in? [coll elm]
            (some #(= % elm) coll))]
    (let [all-optional?            (empty? selection)
          verify-selection?        (and (not (in? #{nil false :skip} verify-selection))
                                        (not all-optional?))
          prune-optionals          (if (not (nil? prune-optionals))
                                     prune-optionals
                                     (-> selection meta :only))
          multi-paths              (when-not all-optional?
                                     (or (::multi-paths options) (multi-branch-keys schema)))
          expanded->origin         (into {}
                                         (mapcat (fn [p]
                                                   (if (some #{{:branch '*}} p)
                                                     (map #(vector % p) (expand-star-branches multi-paths p))
                                                     [[p p]])))
                                         (parse-selection selection))
          selection-paths          (keys expanded->origin)
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
          multi-dispatches         (when keep-only (multi-dispatch-paths schema))
          !required-below          (atom #{})

          walker (let [optionalize-step (fn optionalize-step [v]
                                          (update v 0 mu/optional-keys))
                       require-step     (fn require-step [[schema path :as v]]
                                          (let [cleaned-path (tag-path multi-paths (clean-path path))
                                                to-require   (sel-map cleaned-path)]
                                            (if-not (seq to-require)
                                              v
                                              (let [star? (some #{'*} to-require)]
                                                (record-seen! schema cleaned-path to-require)
                                                (record-prune-exclusions! path)
                                                (if keep-only
                                                  ;; keep, don't require: protect the mentioned
                                                  ;; keys themselves (they stay optional).
                                                  (let [ks (if star?
                                                             (map first (m/entries schema))
                                                             (remove #{'?} to-require))]
                                                    (swap! !prune-exclusions into (map (partial conj path) ks))
                                                    v)
                                                  (update v 0
                                                          #(if star?
                                                             (mu/required-keys %)
                                                             (mu/required-keys % to-require))))))))

                       prune-step  (fn prune-step [[schema path :as v]]
                                     (let [prunable? (cond-> (every-pred (comp :optional second)
                                                                         (comp not @!prune-exclusions #(conj path %) first))
                                                       ;; standalone prune: keep optional entries
                                                       ;; whose subtree has a required attribute.
                                                       keep-only
                                                       (every-pred (comp not @!required-below #(conj path %) first)))
                                           children  (remove prunable? (m/children schema))]
                                       (update v 0 #(m/into-schema (m/type %) (m/-properties %)
                                                                   children (m/-options %)))))
                       ;; bottom-up: mark this map (and its ancestors) when it has a
                       ;; required entry. The dispatch key of a `:multi` branch doesn't
                       ;; count: it is required in any well-formed branch.
                       required-record-step
                       (fn required-record-step [[schema path :as v]]
                         (let [dispatch (when (seq path) (get multi-dispatches (pop path)))]
                           (when (some (fn [[k props _]]
                                         (and (not (:optional props)) (not= k dispatch)))
                                       (m/children schema))
                             (swap! !required-below into (take (inc (count path)) (iterate pop path)))))
                         v)
                       wrap        (fn [stack step]
                                     #(step (stack %)))
                       middlewares (cond-> identity
                                     (not (or optionalized keep-only)) (wrap optionalize-step)
                                     (not all-optional?)               (wrap require-step)
                                     prune-optionals                   (wrap prune-step)
                                     keep-only                         (wrap required-record-step)
                                     :finally                          (wrap first))

                       ;; a `'?` selection at a :multi ({:pet {}}, [{:pet []}]) mentions
                       ;; the multi: mark it seen and protect it from pruning.
                       mention-step (fn mention-step [schema path]
                                      (when-not all-optional?
                                        (let [cleaned    (tag-path multi-paths (clean-path path))
                                              to-require (sel-map cleaned)]
                                          (when (and to-require (to-require '?))
                                            (when verify-selection?
                                              (swap! !seen conj (conj cleaned '?)))
                                            (record-prune-exclusions! path))))
                                      schema)
                       multi-prune-step (fn multi-prune-step [schema path]
                                          (let [children  (m/children schema)
                                                mentioned (filterv #(contains? @!prune-exclusions (conj path (first %)))
                                                                   children)]
                                            (if (seq mentioned)
                                              (m/into-schema (m/type schema) (m/-properties schema)
                                                             mentioned (m/-options schema))
                                              schema)))
                       ;; a keyword :dispatch key must stay required in every explicit
                       ;; map-branch - an "optional" dispatch key is a lie in the form.
                       ;; `::m/default` and nil branches also match on an *absent*
                       ;; dispatch key, so they are left alone.
                       dispatch-require-step
                       (fn dispatch-require-step [schema original]
                         (let [dispatch (:dispatch (m/properties schema))]
                           (if-not (keyword? dispatch)
                             schema
                             (let [orig-entry (fn [dv]
                                                (some (fn [[dv' _ branch]]
                                                        (when (= dv' dv)
                                                          (some (fn [[k p s]] (when (= k dispatch) [p s]))
                                                                (when (= :map (m/type branch)) (m/children branch)))))
                                                      (m/children original)))
                                   fix-child  (fn [[dv props branch :as child]]
                                                (if (or (not= :map (m/type branch))
                                                        (= ::m/default dv)
                                                        (nil? dv))
                                                  child
                                                  (if (mu/get branch dispatch)
                                                    [dv props (mu/required-keys branch [dispatch])]
                                                    (if-let [[eprops eschema] (orig-entry dv)]
                                                      [dv props (mu/assoc branch [dispatch (not-empty (dissoc eprops :optional))] eschema)]
                                                      child))))]
                               (m/into-schema (m/type schema) (m/-properties schema)
                                              (mapv fix-child (m/children schema)) (m/-options schema))))))
                       multi-step (fn multi-step [walked original path]
                                    (-> walked
                                        (mention-step path)
                                        (cond-> prune-optionals (multi-prune-step path))
                                        (dispatch-require-step original)))]
                   (schema-path-walker (comp middlewares vector) multi-step))
          walked (m/walk schema walker
                         {::m/walk-schema-refs true ::m/walk-refs true})]
      (when verify-selection?
        (let [invalid-selection-paths (remove @!seen selection-paths)]
          (when (seq invalid-selection-paths)
            (let [origin-paths (distinct (map #(get expanded->origin % %) invalid-selection-paths))
                  available    (selectable-paths schema)
                  report       (cond-> {:paths     origin-paths
                                        :available (sort-by pr-str
                                                            (into available
                                                                  (star-available multi-paths available origin-paths)))}
                                 (some #(shape-mismatch? multi-paths %) origin-paths)
                                 (assoc :hint (str "a vector selects attributes of a map-schema, "
                                                   "a map selects branches of a multi-schema, "
                                                   "e.g. {:some-branch [:name]}")))]
              (cond
                (fn? verify-selection)    (verify-selection report)
                (= :log verify-selection) (-warn! ::unknown-paths report)
                :else                     (-fail! ::unknown-paths report))))))

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
  - `[{:friends [:name]} {:friends [:age]}]` - require `:name` and `:age` of friends if `:friends` provided (selections merge).

  `:multi` schemas are selected with a *map*: keys are dispatch values, values
  are sub-selections. Unmentioned branches become all-optional. A keyword
  `:dispatch` key stays required in every explicit map-branch:
  - `{:human [:name]}` - require `:name` in branch `:human`.
  - `{'* [:type]}` - require `:type` in *every* branch (fails verification when a branch lacks it).
  - `{'* [:type] :human [:name]}` - `'*` merges with explicit branches.
  - `{::m/default [:x]}` - address the default branch.
  - `{}` - everything optional (like `[]`).
  - `[:id {:pet {:dog [:breed]}}]` - nested: `:pet` is a `:multi` with a `:dog` branch.
  With `prune-optionals`: branches stay when mentioned (or when none is mentioned); other branches are dropped.

  `options`:
  - `verify-selection` - what to do when `selection` contains paths not in `schema`. Defaults to `*verify-selection*` (initially `:throw`):
    - `:throw` (`:assert` works as well) - throw an `ExceptionInfo` with `{:type ::unknown-paths :data {:paths ... :available ...}}` as `ex-data`.
    - `:log` - print a warning (stderr on Clojure, `console.warn` on ClojureScript) and continue.
    - a function - called with `{:paths ... :available ...}`, result ignored, selection continues.
    - `:skip`, `false`, `nil` - don't verify.
  - `prune-optionals` (`false` (default), `true`) - whether all fully optional subtrees should be removed from the resulting schema. Alternatively via metadata of selection: `^:only [:name]` (flag takes precedence over metadata). See also `select-only`, which forces this option, and `prune-optionals`, for pruning an existing schema without selecting.
    Typically used when the selected schema is used for data generation.

  Examples:
  ```
  (select Person)                   ;; all optional
  (select Person [])                ;; all optional
  (select Person ['*])              ;; all root attributes of Person required
  (select Person [:name :handle])   ;; Require specific root attributes.
  (select Person [{:address ['*]}]) ;; Require the full address if provided.

  (select Person [:foo]) ;; Throws ExceptionInfo about non existing path, ex-data contains all possible paths.
  ```
  "
  ([schema]
   (select schema [] nil))
  ([schema selection]
   (select schema selection nil))
  ([schema selection {:as   options
                      :keys [verify-selection prune-optionals]
                      :or   {verify-selection *verify-selection*}}]
   (-select schema selection (assoc options
                                    :verify-selection verify-selection
                                    :prune-optionals prune-optionals))))


(defn select-only
  "Like `select`, but the result contains only the selected attributes:
  `:prune-optionals` is forced to `true` (a caller-supplied
  `:prune-optionals false` is overridden). All other options (e.g.
  `:verify-selection`) pass through unchanged.

  Typically used to generate specific data:
  ```
  (mg/generate (select-only Person [:name])) ;; => {:name \"...\"}
  ```
  For `:multi` schemas, branches the selection does not mention are dropped."
  ([schema selection]
   (select-only schema selection nil))
  ([schema selection options]
   (select schema selection (assoc options :prune-optionals true))))


(defn prune-optionals
  "Remove every optional subtree of `schema` that contains no required
  attribute. Required entries, and optional entries whose subtree contains
  a required attribute, always stay. Typically used to generate samples
  without optional noise from a schema that already is a selection.

  NOTE: `keep-selection` uses the selection notation of `select`, but it
  means KEEP, not require: `(prune-optionals S [:a])` does *not* prune `:a`,
  and kept entries stay optional.

  For `:multi` schemas the branch rule *subtracts*: mentioning one or more
  branches keeps only those branches; mentioning none keeps all. (A keyword
  `:dispatch` key stays required in every kept explicit map-branch.)

  `options` are those of `select`: `:verify-selection` applies to the
  keep-selection; `:prune-optionals` is implied.

  Examples:
  ```
  (prune-optionals Selected)                          ;; only the required spine stays
  (prune-optionals Selected [{:addresses []}])        ;; also keep :addresses as empty optional map
  (prune-optionals Selected [{:addresses [:street]}]) ;; keep :addresses with an OPTIONAL :street
  (prune-optionals Animal {:human []})                ;; keep only branch :human
  ```
  Related: `(select-only S sel)` ≡ `(prune-optionals (select S sel) sel)` —
  the same selection acts as require-spec, then keep-spec."
  ([schema]
   (prune-optionals schema [] nil))
  ([schema keep-selection]
   (prune-optionals schema keep-selection nil))
  ([schema keep-selection {:as   options
                           :keys [verify-selection]
                           :or   {verify-selection *verify-selection*}}]
   (-select schema keep-selection (assoc options
                                         :verify-selection verify-selection
                                         :prune-optionals true
                                         ::keep-only true))))


(defn selector
  "Yields a function similar to `(partial ms/select schema)`.
  A selector is faster when doing multiple selections from a schema as the schema is optionalized once.

  Examples:
  ```
  (let [person-selector (selector Person)]
    (person-selector [:name] {:prune-optionals true})) ;; what `select-only` does
  ```
  "
  [schema]
  (let [optionalized-schema (select schema)
        multi-paths         (multi-branch-keys optionalized-schema)]
    (fn selector-select
      ([selection] (selector-select selection nil))
      ([selection options]
       (-select optionalized-schema selection
                (merge {::optionalized true ::multi-paths multi-paths} options))))))

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

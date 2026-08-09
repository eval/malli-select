# malli-select

[![Clojars Project](https://img.shields.io/clojars/v/dk.thinkcreate/malli-select.svg?include_prereleases)](https://clojars.org/dk.thinkcreate/malli-select) [![cljdoc badge](https://cljdoc.org/badge/dk.thinkcreate/malli-select)](https://cljdoc.org/d/dk.thinkcreate/malli-select) [![Tests](https://github.com/eval/malli-select/actions/workflows/ci.yml/badge.svg)](https://github.com/eval/malli-select/actions/workflows/ci.yml)

Create subschemas of [malli](https://github.com/metosin/malli)-schemas using a spec2-inspired select notation. Works on Clojure and ClojureScript.

It's based on Rich Hickey's ideas from his talk ["Maybe Not"](https://youtu.be/YR5WdGrpoug?feature=shared&t=1965) about how [spec-alpha2](https://github.com/clojure/spec-alpha2) might allow for schema reuse.

## Quickstart

Follow along using [deps-try](https://github.com/eval/deps-try/blob/master/README.md#installation):

``` clojure
$ deps-try io.github.eval/malli-select metosin/malli
# no install/docker
$ docker run -it --rm -v deps-try:/home/deps-try-user ghcr.io/eval/deps-try io.github.eval/malli-select metosin/malli

user=> (require '[malli-select.core :as ms]
                '[malli.core :as m :refer [form] :rename {form p}])
user=> (def Person
         [:map
           [:name string?]
           [:age pos-int?]
           [:addresses [:vector [:map
                                  [:street string?] [:zip string?]]]]])

;; require :name, everything else is optional
user=> (p (ms/select Person [:name]))
[:map
 [:name string?]
 [:age {:optional true} pos-int?]
 [:addresses
  {:optional true}
  [:vector
   [:map
    [:street {:optional true} string?]
    [:zip {:optional true} string?]]]]]

;; *if* any address is provided, it should at least have :street
user=> (p (ms/select Person [{:addresses [:street]}]))
[:map
 [:name {:optional true} string?]
 [:age {:optional true} pos-int?]
 [:addresses
  {:optional true}
  [:vector
   [:map [:street string?] [:zip {:optional true} string?]]]]]

;; example valid data:
;; {}, {:addresses []}, {:addresses [{:street "Main"}]}
;;
;; example invalid data:
;; {:addresses nil}, {:addresses [{}]}, {:addresses [{:street "Foo" :zip 1234}]}

;; any address provided should be a full address
user=> (p (ms/select Person [{:addresses ['*]}]))
;;
;; require all attributes of a person (shallow, i.e. address attributes become optional)
user=> (p (ms/select Person ['*]))
;; example valid data:
;; {:name "Foo" :age 18 :addresses [{}]}


;; remove any optional attribute
user=> (p (ms/select-only Person [{:addresses ['*]}]))
;; example valid data:
;; {:name :not-a-string}
;;
;; Typically you'd use this to generate only specific data:
user=> (require '[malli.generator :as mg])
user=> (mg/generate (ms/select-only Person [:name]))
{:name "sNeLdUI5KtPw"}
;; also supported: (ms/select Person [:name] {:prune-optionals true})
;; and, via metadata: (ms/select Person ^:only [:name])

;; selecting something not contained in the schema:
user=> (ms/select Person [:a])
Execution error (ExceptionInfo) at malli-select.core/-fail! (core.cljc:7).
:malli-select.core/unknown-paths {:paths ([:a]), :available ([:addresses :street] [:addresses :zip] [:addresses] [:age] [:name])}

;; the unknown and available paths are also in the ex-data:
user=> (ex-data *e)
{:type :malli-select.core/unknown-paths,
 :data {:paths ([:a]),
        :available ([:addresses :street] [:addresses :zip] [:addresses] [:age] [:name])}}

;; bypass this check:
user=> (ms/select Person [:a] {:verify-selection false})
;; :verify-selection defaults to :throw (`:assert`, the pre-v0.8 spelling, still works);
;; :skip, nil and false disable the check.

;; other options: :log warns (stderr/console.warn) and continues...
user=> (ms/select Person [:a] {:verify-selection :log})
WARNING: :malli-select.core/unknown-paths {:paths ([:a]), :available (...)}
;; ...and a function gets the report, e.g.:
user=> (ms/select Person [:a] {:verify-selection #(log/warn "unknown paths" (:paths %))})

;; change the default via the dynamic var ms/*verify-selection*:
user=> (alter-var-root #'ms/*verify-selection* (constantly :log)) ;; CLJS: (set! ms/*verify-selection* :log)
```

> [!NOTE]
> Before v0.8 an invalid selection threw an `AssertionError` instead of an
> `ExceptionInfo` — see the [CHANGELOG](CHANGELOG.md) if you were catching it.

See [the tests](./test/malli_select/core_test.cljc) for more.


## Multi-schemas

Branches of a `:multi` schema are selected with a *map*: keys are dispatch
values, values are sub-selections. The selection mirrors the shape of the
schema — a vector addresses attributes of a `:map`, a map addresses branches
of a `:multi` — at any nesting depth.

``` clojure
user=> (def Animal
         [:multi {:dispatch :type}
           [:human [:map [:type :keyword] [:name string?] [:age pos-int?]]]
           [:sized [:map [:type :keyword] [:size pos-int?]]]])

;; require :name in the :human branch; other branches become all-optional
user=> (p (ms/select Animal {:human [:name]}))
[:multi {:dispatch :type}
 [:human [:map [:type :keyword] [:name string?] [:age {:optional true} pos-int?]]]
 [:sized [:map [:type :keyword] [:size {:optional true} pos-int?]]]]

;; note that :type stays required: a keyword :dispatch key is auto-required in
;; every explicit map-branch, as data without it won't dispatch anyway.
;; (`::m/default` and nil branches are left alone - they also match data
;; *without* a dispatch value.)

;; '* addresses all branches...
user=> (p (ms/select Animal {'* [:type]}))

;; ...so the sub-selection must be satisfiable in *every* branch:
user=> (ms/select Animal {'* [:name]})
Execution error (ExceptionInfo) ...
;; :available contains what all branches have in common, e.g. [{:branch *} :type]

;; be more precise ('* merges with explicit branches):
user=> (ms/select Animal {'* [:type] :human [:name]})
;; ...or skip verification (branches lacking :name are then left as-is):
user=> (ms/select Animal {'* [:name]} {:verify-selection false})

;; a nested multi (a vector selection addresses map-attributes,
;; a map selection addresses branches):
user=> (def Person [:map [:id :int] [:pet [:multi {:dispatch :kind}
                                            [:dog [:map [:kind :keyword] [:breed string?]]]
                                            [:cat [:map [:kind :keyword] [:lives :int]]]]]])
user=> (p (ms/select Person [:id {:pet {:dog [:breed]}}]))

;; select-only drops unmentioned branches (mentioning none keeps all),
;; e.g. to generate only humans:
user=> (mg/generate (ms/select-only Animal {:human [:name]}))
{:name "x2Ep", :type :human}
```

Limitations:
- Branches that are not `:map` schemas (e.g. `[:multi ... [:str :string]]`) pass through untouched and cannot be selected.
- Numeric dispatch values `0` and `1` are unsupported (they collide with how paths are cleaned internally).
- A function-valued `:dispatch` disables the dispatch-key auto-require.


## Pruning

`select-only` needs schema and selection at hand. When a schema already *is*
a selection — or is hand-written with optional keys — use `prune-optionals`: it removes
every optional subtree that contains no required attribute.

``` clojure
user=> (def Selected
         (ms/select [:map
                     [:name string?]
                     [:age pos-int?]
                     [:addresses [:vector [:map
                                           [:street string?]
                                           [:zip string?]]]]]
                    [:name]))

user=> (p (ms/prune-optionals Selected))
[:map [:name string?]]

;; the second argument is a *keep*-selection: it reuses the selection
;; notation, but mentioned entries are KEPT, not required - they stay optional:
user=> (p (ms/prune-optionals Selected [{:addresses []}]))
[:map
 [:name string?]
 [:addresses {:optional true} [:vector :map]]]

user=> (p (ms/prune-optionals Selected [{:addresses [:street]}]))
[:map
 [:name string?]
 [:addresses {:optional true}
  [:vector [:map [:street {:optional true} string?]]]]]

;; multi-schemas: mentioning one or more branches keeps only those branches,
;; mentioning none keeps all. (Without this rule a pruned Animal would keep
;; generating bare `{:type :sized}` samples.)
user=> (p (ms/prune-optionals Animal {:human []}))
[:multi {:dispatch :type}
 [:human [:map [:type :keyword] [:name string?] [:age pos-int?]]]]
```

Optional entries whose subtree contains a required attribute survive a prune —
e.g. hand-written `[:addresses {:optional true} [:map [:street :string]]]`
keeps `:addresses` (and its required `:street`).

Related: `(ms/select-only S sel)` ≡ `(ms/prune-optionals (ms/select S sel) sel)` — the
same selection acts as require-spec, then keep-spec.


## LICENSE

Copyright (c) 2026 Gert Goet, ThinkCreate.
Distributed under the MIT license. See [LICENSE](LICENSE).

# malli-select

[![Clojars Project](https://img.shields.io/clojars/v/dk.thinkcreate/malli-select.svg?include_prereleases)](https://clojars.org/dk.thinkcreate/malli-select) [![cljdoc badge](https://cljdoc.org/badge/dk.thinkcreate/malli-select)](https://cljdoc.org/d/dk.thinkcreate/malli-select) [![Tests](https://github.com/eval/malli-select/actions/workflows/ci.yml/badge.svg)](https://github.com/eval/malli-select/actions/workflows/ci.yml)

Create subschemas of [malli](https://github.com/metosin/malli)-schemas using a spec2-inspired select notation.

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
    [:country {:optional true} string?]]]]]

;; *if* any address is provided, it should at least have :street
user=> (p (ms/select Person [{:addresses [:street]}]))
[:map
 [:name {:optional true} string?]
 [:age {:optional true} pos-int?]
 [:addresses
  {:optional true}
  [:vector
   [:map [:street string?] [:country {:optional true} string?]]]]]

;; example valid data:
;; {}, {:addresses []}, {:addresses [{:street "Main"}]}
;;
;; example invalid data:
;; {:addresses nil}, {:addresses [{}]}, {:addresses [{:street "Foo" :country :se}]}

;; any address provided should be a full address
user=> (p (ms/select Person [{:addresses ['*]}]))
;;
;; require all attributes of a person (shallow, i.e. address attributes become optional)
user=> (p (ms/select Person ['*]))
;; example valid data:
;; {:name "Foo" :age 18 :addresses [{}]}


;; remove any optional attribute
user=> (p (ms/select Person [{:addresses ['*]}] {:prune-optionals true}))
;; or shorter:
user=> (p (ms/select Person ^:only [{:addresses ['*]}]))
;; example valid data:
;; {:name :not-a-string}
;;
;; Typically you'd use this to generate only specific data:
user=> (require '[malli.generator :as mg])
user=> (mg/generate (ms/select Person ^:only [:name]))
{:name "sNeLdUI5KtPw"}

;; selecting something not contained in the schema:
user=> (ms/select Person [:a])
Execution error (ExceptionInfo) at malli-select.core/-fail! (core.clj:7).
:malli-select.core/unknown-paths {:paths ([:a]), :available ([:addresses :street] [:addresses :zip] [:addresses] [:age] [:name])}

;; the unknown and available paths are also in the ex-data:
user=> (ex-data *e)
{:type :malli-select.core/unknown-paths,
 :data {:paths ([:a]),
        :available ([:addresses :street] [:addresses :zip] [:addresses] [:age] [:name])}}

;; bypass this check:
user=> (ms/select Person [:a] {:verify-selection false})
```

See [the tests](./test/malli_select/core_test.clj) for more.


## LICENSE

Copyright (c) 2026 Gert Goet, ThinkCreate.
Distributed under the MIT license. See [LICENSE](LICENSE).

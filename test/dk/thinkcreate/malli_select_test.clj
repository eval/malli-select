(ns dk.thinkcreate.malli-select-test
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :as t :refer [deftest is testing]]
   [dk.thinkcreate.malli-select :refer [select]]
   [malli.core :as m]
   [malli.util :as mu]))

(defonce ^:private ^:dynamic
  *schema* nil)

(defn pps [o]
  (with-out-str (pprint o)))

(defmacro expect-selection-to-validate [sel & data+maybe-reason]
  `(if ~sel
     (let [data#       ~(first data+maybe-reason)
          sel-schema# (select *schema* ~sel (meta ~sel))
          result#     (or (m/validate sel-schema# data#) (m/explain sel-schema# data#))]
      (is (true? result#)
          (cond-> (str "Expected data:\n" (pps data#) "to be valid given schema:\n" (pps (m/form sel-schema#)))
            ~(second data+maybe-reason) (str "because:\n" ~(second data+maybe-reason))
            :always                     (str "\nvalidate errors:\n" (pps (:errors result#))))))
     *schema*))

(defmacro expect-selection-to-invalidate [sel & data+maybe-reason]
  `(if ~sel
     (let [data#       ~(first data+maybe-reason)
          sel-schema# (select *schema* ~sel (meta ~sel))]
      (is (false? (m/validate sel-schema# data#))
          (cond-> (str "Expected data:\n" (pps data#) "to be *invalid* given schema:\n" (pps (m/form sel-schema#)))
            ~(second data+maybe-reason) (str "because:\n" ~(second data+maybe-reason)))))
     *schema*))


(deftest select-test
  (testing "common selections"
    (let [S1 [:map
              [:name string?]
              [:handle string?]
              [:address [:maybe [:map
                                 [:street string?]
                                 [:zip int?]
                                 [:country [:map-of
                                            string? [:map
                                                     [:iso string?] [:name string?]]]]]]]
              [:roles [:set [:map
                             [:name string?]]]]]]
      (testing "all optional"
        (binding [*schema* S1]
          (expect-selection-to-validate   [] {})
          (expect-selection-to-validate   [] {:address {:country {}}})
          (expect-selection-to-invalidate [] {:address {:country {:dk {}}}}
                                          "types should still match")))

      (testing "root attributes"
        (binding [*schema* S1]
          (expect-selection-to-invalidate [:name] {})
          (expect-selection-to-validate   [:name] {:name "Gert"})
          (expect-selection-to-invalidate [:handle :name] {:name "Gert"})
          (expect-selection-to-validate   [:name :handle] {:name "Gert" :handle "eval"})
          (expect-selection-to-invalidate [:address] {}
                                          ":address required")
          (expect-selection-to-validate   [:address] {:address nil}
                                          ":address key provided - nothing needed from address"))

        ;; wrapped
        (binding [*schema* [:maybe [:vector S1]]]
          (expect-selection-to-validate   nil nil "normal malli behavior")
          (expect-selection-to-validate   nil [{}] "normal malli behavior")
          (expect-selection-to-invalidate [:name] [{}])
          (expect-selection-to-validate   [:name] [{:name "Gert"}])
          (expect-selection-to-invalidate [:handle :name] [{:name "Gert"}])
          (expect-selection-to-validate   [:name :handle] [{:name "Gert" :handle "eval"}])))

      (testing "nested attributes"
        (binding [*schema* S1]
          (expect-selection-to-validate   [{:address [:street]}] {} ":address not provided")
          (expect-selection-to-invalidate [{:address [:street]}] {:address {}})
          (expect-selection-to-invalidate [:address {:address [:street]}] {})

          (expect-selection-to-validate   [:address {:address [:street]}] {:address {:street "Main"}}
                                          "the root :address should not interfere with the nested selection")
          (expect-selection-to-validate   [{:address [:street]} :address] {:address {:street "Main"}}
                                          "order of items should not matter here")

          ;; star selection
          (expect-selection-to-validate   [{:address ['*]}] {} ":address not provided")
          (expect-selection-to-invalidate [{:address ['*]}] {:address {:street "Main"}})
          (expect-selection-to-validate   [{:address ['*]}] {:address {:street  "Main"
                                                                       :zip     1234
                                                                       :country {}}}
                                          "all keys of address-map should be required - contents of country optional!")
          (expect-selection-to-validate   [{:address ['* {:country []}]}]
                                          {:address {:street  "Main"
                                                     :zip     1234
                                                     :country {}}}
                                          "override star-selection: country-content is optional")

          ;; selecting through a set
          (expect-selection-to-validate   [{:roles [:name]}] {:roles #{}} "no role provided")
          (expect-selection-to-invalidate [{:roles [:name]}] {:roles #{{}}})
          (expect-selection-to-validate   [{:roles [:name]}] {:roles #{{:name "Admin"}}})

          (expect-selection-to-validate [{:address [:street]} {:address [:zip]}] {:address {:zip 1234}}
                                        "last selection of :address wins")

          (expect-selection-to-invalidate [{:address [:street] :roles [:name]}
                                           {:address [:zip]}]
                                          {:address {:zip 1234} :roles [{}]}
                                          "selection maps are merged")
          (expect-selection-to-validate [{:address [:street] :roles [:name]}
                                         {:address [:zip]}]
                                        {:address {:zip 1234} :roles #{{:name "Admin"}}}
                                        "[:address :street] is overridden by [:address :zip]")

          (expect-selection-to-validate [{:address [{:country [:name]}]}]
                                        {:address {:country {"DK" {:name "Denmark"}}}}
                                        "multi-level nesting")))))
  (testing "schema with refs"
    (let [S1 [:schema {:registry {"Other" [:map
                                           [:other boolean?]]}}
              [:map
               [:this boolean?]
               [:that "Other"]]]]

      (testing "marking all optional"
        (binding [*schema* S1]
          (expect-selection-to-validate [] {})
          (expect-selection-to-validate [] {:that {}} "it should walk refs")))

      (testing "star selections"
        (binding [*schema* (select S1 [])] ;; ensure all optional
          (expect-selection-to-invalidate ['*] {})
          (expect-selection-to-validate   ['*] {:this true :that {}})
          (expect-selection-to-invalidate [{:that ['*]}] {:that {}}
                                          "it should walk refs and require all keys from Other")
          (expect-selection-to-validate   [{:that ['*]}] {:that {:other true}}
                                          "it has all keys of Other-schema")))))

  (testing "options"
    (testing "prune-optionals"
      (testing "common"
        (binding [*schema* [:map
                            [:name string?]
                            [:age int?]
                            [:addresses [:maybe [:vector [:map
                                                          [:street string?]
                                                          [:zip int?]]]]]]]
          (expect-selection-to-validate  ^:only [:name]
                                         {:name "Gert" :age "N/A" :addresses 1}
                                         ":age can now be anything as it's no longer part of the schema")
          (expect-selection-to-invalidate ^:only [{:addresses [:street]}]
                                          {:addresses [{}]}
                                          "optional :addresses doesn't trigger a deep prune")
          (expect-selection-to-validate ^:only [:addresses {:addresses [:street]}]
                                        {:addresses [{:street "Main"}]})))
      (testing "optionals with aggregates"
        (binding [*schema* [:maybe [:map
                                    [:address [:map [:street string?]]]
                                    [:friends [:maybe [:vector [:map [:name string?]]]]]
                                    [:countries [:map-of string? [:map [:name string?]]]]]]]
          (expect-selection-to-validate ^:only []
                                        {:countries 1}
                                        ":countries is no longer part of schema")
          (expect-selection-to-invalidate ^:only [:countries]
                                          {:countries 1}
                                          ":countries should require a map-of string map")
          (expect-selection-to-invalidate ^:only [:countries]
                                          {:countries {"foo" 1}}
                                          ":countries should require a map-of string map")
          (expect-selection-to-validate ^:only [:countries]
                                        {:countries {"foo" {}}}
                                        ":countries is no longer part of the schema")

          (expect-selection-to-validate ^:only [:friends]
                                        {:friends [{}]}
                                        ":friends requires just a vector of maps")
          (expect-selection-to-invalidate ^:only [:friends]
                                          {:friends [1]}
                                          ":friends requires just a vector of maps")))
      (testing "schema with refs"
        (binding [*schema* [:schema {:registry {"Other" [:map
                                                         [:other boolean?]]}}
                            [:map
                             [:this boolean?]
                             [:that "Other"]]]]
          (expect-selection-to-validate ^{:prune-optionals true} []
                                        {:that {:other "?"}}
                                        ":other can be a string as it should no longer be part of the schema"))))
    (testing "verify-selection"
      (is (thrown-with-msg? AssertionError #"unknown paths: \(\[:a\]\)"
                            (select int? [:a])))
      (testing "disabling it"
        (is (some? (select int? [:a] {:verify-selection :skip})))
        (is (some? (select int? [:a] {:verify-selection nil})))))))

(comment
  (select [:map [:address [:map [:street string?]]]] [{:address [:street]}] {:prune-optionals true})

  (mu/update-properties :map assoc :closed true)
  (m/validate [:map {:closed true}] {})
  )

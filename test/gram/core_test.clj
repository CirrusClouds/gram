(ns gram.core-test
  (:require [gram.core :as gram]
            [malli.core :as mal]
            [clojure.test :refer :all]))

(deftest parsing-tests
  (testing "Parsing works?"))

(deftest schema-malli-tests
  (testing "type coercion of various types"
    (let [types-by-name {"letter"
                         {:juxt.grab.alpha.graphql/kind 'ENUM
                          :juxt.grab.alpha.graphql/enum-values
                          [{:juxt.grab.alpha.graphql/name "a"}
                           {:juxt.grab.alpha.graphql/name "b"}
                           {:juxt.grab.alpha.graphql/name "c"}]}
                         "either-or" {:juxt.grab.alpha.graphql/kind 'UNION
                                      :juxt.grab.alpha.graphql/member-types ["letter" "Int"]}
                         "Int" {:juxt.grab.alpha.graphql/kind 'SCALAR
                                :juxt.grab.alpha.graphql/name "Int"}}]
      (is (= :int (gram/schema->malli* types-by-name (get types-by-name "Int"))))
      (is (= [:enum "a" "b" "c"] (gram/schema->malli* types-by-name (get types-by-name "letter"))))
      (is (= [:or [:enum "a" "b" "c"] :int] (gram/schema->malli* types-by-name (get types-by-name "either-or")))))))

(deftest query-malli-tests
  (let [malli-schema {"a" :int
                      "b" [:enum "a" "b" "c"]
                      "c" [:or :int [:enum "a" "b" "c"]]}
        operations-by-name {"create-a"
                            {:juxt.grab.alpha.graphql/operation-type "mutation"
                             :juxt.grab.alpha.graphql/name "create-a"
                             :juxt.grab.alpha.graphql/variable-definitions
                             [{:juxt.grab.alpha.graphql/variable "data"
                               :juxt.grab.alpha.graphql/type-ref
                               {:juxt.grab.alpha.graphql/name "a"}}]}
                            "create-a-2"
                            {:juxt.grab.alpha.graphql/operation-type "mutation"
                             :juxt.grab.alpha.graphql/name "create-a-2"
                             :juxt.grab.alpha.graphql/variable-definitions
                             [{:juxt.grab.alpha.graphql/variable "data"
                               :juxt.grab.alpha.graphql/type-ref
                               {:juxt.grab.alpha.graphql/non-null-type
                                {:juxt.grab.alpha.graphql/name "a"}}}]}
                            "create-c"
                            {:juxt.grab.alpha.graphql/operation-type "mutation"
                             :juxt.grab.alpha.graphql/name "create-c"
                             :juxt.grab.alpha.graphql/variable-definitions
                             [{:juxt.grab.alpha.graphql/variable "data"
                               :juxt.grab.alpha.graphql/type-ref
                               {:juxt.grab.alpha.graphql/non-null-type
                                {:juxt.grab.alpha.graphql/list-type
                                 {:juxt.grab.alpha.graphql/name "c"}}}}]}}]
    (testing "coercion of queries"
      (is (=
           {:operation-type "mutation"
            :variables {"data"
                        {:schema :int
                         :optional true}}}
           (gram/query->malli* (get operations-by-name "create-a") malli-schema)))
      (is (=
           {:operation-type "mutation"
            :variables {"data" {:schema :int}}}
           (gram/query->malli* (get operations-by-name "create-a-2") malli-schema)))
      (is (=
           {:operation-type "mutation"
            :variables {"data" {:schema [:sequential [:or :int [:enum "a" "b" "c"]]]}}}
           (gram/query->malli* (get operations-by-name "create-c") malli-schema))))))

(deftest e2e-test
  (testing "end-to-end"
    (let [schema "test-resources/schema-1.graphql"
          document "test-resources/query-1.graphql"
          result (gram/graphql->malli schema document)]
      (is (mal/validate (-> (get result "CreateCat")
                            :variables
                            (#(get % "data"))
                            :schema
                            mal/schema)
                        {:id "blah-cat"
                         :name "blah-cat"
                         :breed "Idk"
                         :likes ["Sitting" "Sleeping"]})))))

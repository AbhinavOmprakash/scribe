(ns scribe.core-test
  (:require [clojure.test :refer :all]
            [scribe.core :refer [record-related-syms select-query defmodel]]))


(defmodel foo1
  {:table :foo1
   :pk :foo1.a
   :record
   {:a int?
    :b int?}})

(defmodel foo2
  {:table :foo2
   :pk :foo2.c
   :record
   {:c int?
    :d int?}})


(defmodel foo3
  {:table :foo3
   :pk :foo3.id
   :record {:id int?
            :e int?
            :foo1 {:model foo1
                   :relationship :one-to-one
                   :fk :foo1.a}}})

(deftest test-record-related-syms
  (is (=  (record-related-syms foo1 {:prefix 'map->})
          'scribe.core-test/map->foo1))
  (is (=  (record-related-syms scribe.core_test.foo1 {:prefix 'map->})
          'scribe.core-test/map->foo1)
      "Can handle symbols with underscores")
  (is (=  (record-related-syms scribe.core_test.foo1 {:prefix 'map->
                                                      :suffix '->vec})
          'scribe.core-test/map->foo1->vec)
      "Can handle symbols with underscores"))

(deftest test-select-query
  (testing "select query generation for one-to-one relationships"
    (is (= (select-query foo1)
           {:select [:foo1.a :foo1.b], :from [:foo1]}))
    (is (= (select-query foo3)
           {:select
            [:foo3.id
             :foo3.e
             [{:select [[[:to_json :child]]],
               :from [[{:select [:foo1.a :foo1.b], :from [:foo1]} :child]],
               :where [:= :foo3.id :foo1.a]}
              :foo1]],
            :from [:foo3]}))))

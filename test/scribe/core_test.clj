(ns scribe.core-test
  (:require [clojure.test :refer :all]
            [scribe.models-test :refer [Author-spec]]
            [scribe.core :refer [record-related-syms select-query defmodel spec]]
            [clojure.string :as string])
  (:import (scribe.models_test Author Category Post)))

(defrecord foo1 [])

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

(deftest test-spec
  (is (= (spec Author)
         Author-spec))
  (is (= (spec (Author. 1 "Author"))
         Author-spec)))


(deftest test-select-query
  (testing "select query generation for one-to-one relationships"
    (is (= (select-query Author)
           {:select [:blog.author.id :blog.author.name],
            :from [:blog.author]}))
    (is (= (select-query Post)
           {:select
            [:blog.post.id
             :blog.post.title
             :blog.post.body
             :blog.post.published_on
             [{:select [[[:to_json :child]]],
               :from
               [[{:select [:blog.author.id :blog.author.name],
                  :from [:blog.author]}
                 :child]],
               :where [:= :post.id :author.id]}
              :author]
             [{:select [[[:json_agg :item]]],
               :from
               [[{:select [:blog.category.id :blog.category.name],
                  :from [:blog.category],
                  :join
                  [[:category_and_posts]
                   [:= :category_and_posts.category_id :category.id]],
                  :where [:= :category_and_posts.post_id :post.id]}
                 :item]]}
              :category]],
            :from [:blog.post]}))))


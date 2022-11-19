(ns scribe.core-test
  (:require [clojure.test :refer :all]
            [scribe.models-test :refer [Author-spec
                                        map->Post
                                        map->Author
                                        map->Editor
                                        map->Publication]]
            [scribe.core :refer [record-related-syms
                                 select-query
                                 defmodel
                                 spec
                                 unnest]]
            [clojure.string :as string])
  (:import (scribe.models_test Author Category Post Publication)))

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
         (Author-spec)))
  (is (= (spec (map->Author {:id 1 :name "Author"}))
         (Author-spec))))

;; write test for unnest when map has more keys than the record does 
;; and when a record column is a derived-column. 
(deftest test-unnest
  (testing "nil behaviour"
    (is (= (unnest Author nil)
           {:blog.author {:value {}, :pk :author.id}}))
    (is (thrown? java.lang.AssertionError (unnest nil nil))))

  (testing "Simple case of unnest, where models have one-to-one relationships. "
    (let [author1 (map->Author {:id 1 :name "Author"})
          post1 (map->Post {:id 1 :author author1})]
      (is (= (unnest Author author1)
             {:blog.author {:value {:id 1, :name "Author"}, :pk :author.id}}))
      (is (= (unnest Post post1)
             {:blog.post
              {:value {:id 1, :title nil, :body nil, :published_on nil},
               :pk :post.id},
              :blog.author {:value {:id 1, :name "Author"}, :pk :author.id},
              :blog.category {:value {}, :pk :category.id}}))))

  (testing "Simple case of unnest, where multiple objects of the same model are passed."
    (let [author1 (map->Author {:id 1 :name "Author 1"})
          author2 (map->Author {:id 2 :name "Author 2"})
          author3 (map->Author {:id 3 :name "Author 3"})]
      (is (= (unnest Author author1 author2 author3)
             {:blog.author
              [{:value {:id 1, :name "Author 1"}, :pk :author.id}
               {:value {:id 2, :name "Author 2"}, :pk :author.id}
               {:value {:id 3, :name "Author 3"}, :pk :author.id}]}))))

  (testing "Unnest can handle one-to-many nested objects"
    (let [ed1 (map->Editor {:id 1})
          ed2 (map->Editor {:id 2})
          ed3 (map->Editor {:id 3})
          pub (map->Publication {:id 1 :name "Publication" :editors [ed1 ed2 ed3]})]
      (is (= (unnest Publication pub)
             {:blog.publication
              {:value {:id 1, :name "Publication"}, :pk :publication.id},
              :blog.editor
              [{:value {:id 1, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 2, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 3, :name nil, :publication nil}, :pk :editor.id}]}))))
  (testing "Unnest can handle multiple one-to-many nested objects"
    (let [ed1 (map->Editor {:id 1})
          ed2 (map->Editor {:id 2})
          ed3 (map->Editor {:id 3})
          pub1 (map->Publication {:id 1 :name "Publication" :editors [ed1 ed2 ed3]})
          ed4 (map->Editor {:id 4})
          ed5 (map->Editor {:id 5})
          ed6 (map->Editor {:id 6})
          pub2 (map->Publication {:id 2 :name "Publication 2" :editors [ed4 ed5 ed6]})]
      (is (= (unnest Publication pub1 pub2)
             {:blog.publication
              [{:value {:id 1, :name "Publication"}, :pk :publication.id}
               {:value {:id 2, :name "Publication 2"}, :pk :publication.id}],
              :blog.editor
              [{:value {:id 1, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 2, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 3, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 4, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 5, :name nil, :publication nil}, :pk :editor.id}
               {:value {:id 6, :name nil, :publication nil}, :pk :editor.id}]})))))

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


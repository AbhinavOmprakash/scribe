(ns scribe.models-test
  (:gen-class)
  (:require [scribe.core :refer [defmodel declare-model]]))

;; Going to model a blog 

(declare-model Category Post)

(defmodel Author
  {:table :blog.author
   :pk :author.id
   :record {:id int?
            :name string?
            ;; For iteration 1 lets NOT model bidirectional relationships
            #_:posts #_{:model Post
                        :relationship :one-to-many
                        :fk :post.id}}})

;; going to assume one post can only have one author

(defmodel Post
  {:table :blog.post
   :pk :post.id
   :record {:id int?
            :title string?
            :body string?
            :published_on inst?
            :author {:model Author
                     :relationship :one-to-one
                     :fk :author.id}
            :category {:model Category
                       :relationship :many-to-many
                       :fk :category.id
                       :mapping-table :category_and_posts
                       :mapping-table-fk :category_and_posts.category_id
                       :mapping-table-pk :category_and_posts.post_id}}})

(defmodel Category
  {:table :blog.category
   :pk :category.id
   :record {:id int?
            :name string?
;; For iteration 1 lets NOT model bidirectional relationships
            #_:posts #_{:model Post
                        :relationship :many-to-many
                        :fk :post.id
                        :mapping-table :category_and_posts
                        :mapping-table-fk :category_and_posts.post_id
                        :mapping-table-pk :category_and_posts.category_id}}})

(defmodel Editor 
  {:table :blog.editor
   :pk :editor.id 
   :record {:id int?
            :name string?
            :publication int?}})

;; Editors can only work for one Publication at a time.
(defmodel Publication
  {:table  :blog.publication
   :pk     :publication.id
   :record {:id    int?
            :name  string?
            :editors {:model Editor 
                      :relationship :one-to-many 
                      :fk :editor.publication
                      }}})

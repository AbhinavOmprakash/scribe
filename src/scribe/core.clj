(ns scribe.core
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [scribe.postgres :as pg]))
(require '[snitch.core :refer [defn* defmethod*]])


;; useful for writing macros
(defn concat-symbols
  "Concatenates all the symbols passed."
  [& syms]
  (->> syms
       (apply str)
       symbol))


(defmacro declare-model
  "Like declare but for records. 
  declare creates vars and you can't redefine a var as a record."
  [& models]
  (cons 'do
        (for [m models]
          `(defrecord ~m []))))


;; protocol that all models will implement

(defprotocol SModel
  (spec [this]))

(defn spec-name [name]
  (concat-symbols name '-spec))

(defmacro defmodel [name {:keys [record] :as spec*}]
  (let [fields (->> record
                    keys
                    (mapv symbol))]
    `(do (defn ~(spec-name name) [] ~spec*)
         (defrecord ~name ~fields
           SModel
           (~'spec [~'this] ~spec*)))))



(defn record-related-syms
  ;; hacky 
  ;; i want to get the symbols related to a record
  ;; I wouldn't have to do this if there was a way to define a static 
  ;; method on a record. 
  [record-sym {:keys [prefix suffix]}]
  ;; WARNING: hacky code ahead. this feels dirty.
  (let [parts (-> record-sym
                  str
                  (str/split #"\s")
                  last
                  (str/split #"\."))
        record-name (last parts)
        record-ns (str/join "." (map (fn [s]
                                       (str/replace s #"_" "-"))  ; replace underscores with dashes
                                     (butlast parts)))
        sym (symbol record-ns (str (concat-symbols prefix record-name suffix)))]
    sym))

(defn spec
  "Returns the spec for model `model`. 
  `model` can be a java class 
  or the instantiated record of that class.

   
  Example Usage 
  (defmodel a-model
    {:table :model
    :pk :id
    :record {:id int?}})
  (spec a-model)
  ;; => {:table :model, :pk :id, :record {:id #function[clojure.core/int?]}}
  
  (spec #scribe.core.a-model{:id 1})
  ;; => {:table :model, :pk :id, :record {:id #function[clojure.core/int?]}}
  "
  [model]
  (if (satisfies? SModel model)
    (.spec model)
    ((resolve (record-related-syms model {:suffix '-spec})))))

(comment
  ;; usage of `spec` 
  (defmodel a-model
    {:table :model
     :pk :id
     :record {:id int?}})
  (spec a-model)
  ;; => {:table :model, :pk :id, :record {:id #function[clojure.core/int?]}}

  (spec #scribe.core.a-model{:id 1})
  ;; => {:table :model, :pk :id, :record {:id #function[clojure.core/int?]}}
  )


(defn construct-record [record-sym]
  ((resolve (record-related-syms record-sym {:prefix 'map->}))
   {}))


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
    :d int?
    :foo1 {:model foo1
           :relationship :one-to-one
           :fk :a}}})


(-> (foo2-spec)
    :record
    :foo1
    :model)

(defn join-table?
  [column-spec]
  (and (map? column-spec)
       (contains? column-spec :relationship)))

(defn keys-for-joined-tables
  ;; rewrite docstring.
  "Returns a set of keys for joined tables."
  [{:keys [record] :as _spec}]
  (reduce-kv (fn [acc k v]
               (if (join-table? v)
                 (conj acc k)
                 acc))
             #{}
             record))

(defn derived-column?
  [column-spec]
  (and (map? column-spec)
       (contains? column-spec :projection)))


(defn one-to-one?
  [{:keys [relationship]}]
  (= relationship :one-to-one))


(defn one-to-many?
  [{:keys [relationship]}]
  (= relationship :one-to-many))


(defn many-to-many?
  [{:keys [relationship]}]
  (= relationship :many-to-many))


(defn qaulified-column
  [table column]
  (let [column* (name column)
        column**  (if (str/includes? column* ".")
                    (-> column*
                        (str/split #"\.")
                        last)
                    column*)
        table* (name table)]
    (keyword (str  table* "." column**))))

(comment
  (qaulified-column :foo :bar) ; :foo.bar
  )

(defn model-spec? [{:keys [model] :as m}]
  (and model (map? m)))


(defn join
  [select-q pk {:keys [model column fk mapping-table-fk mapping-table-pk mapping-table] :as record}]
  (let [pk* (or column pk)] ; if the child maps to a column in the parent table, that is the column to join the fk on
    (cond
      (one-to-one? record) (pg/one-to-one select-q pk* fk)
      (one-to-many? record) (pg/one-to-many select-q pk* fk)
      (many-to-many? record) (pg/many-to-many select-q
                                              pk*
                                              fk
                                              mapping-table
                                              mapping-table-pk
                                              mapping-table-fk))))

;; select query 
(declare select-query)

(defn qualified-projections
  [{:keys [table pk record] :as spec}]
  (reduce-kv (fn [acc k v]
               (if (join-table? v)
                 (conj acc [(join (select-query (:model v)) pk v) k])
                 (conj acc (qaulified-column table k))))
             []
             record))


(defn select-query
  "Generates a select query for `model`.
  Hydrates all related objects."
  [model]
  (let [{:keys [table record]
         :as spec} (spec model)]
    {:select (qualified-projections spec)
     :from [table]}))

(defn record [model]
  "Extract the record of a model"
  (if (map? model)
    (:record model)
    (:record (spec model))))

;; update query 

(defn* unnest
  "The unnest algorithm
  1. The tree is the parent object. Select all the keys from the parent object corresponding to the 
  keys in the record. The record is the source of truth for the db.
  2. In order to unnest an object, we need to know which kv pairs refer to other tables. 
     We refer to the model's spec for this
  "
  [model tree]
  (let [spec* (spec model)
        tree* (select-keys tree (keys (record model)))
        table* (:table spec*)
        join-ks (keys-for-joined-tables spec*)]
    (reduce-kv (*fn [acc k v]
                    (if (contains? join-ks k)
                      (let [child-model (-> spec* record k :model)]
                        (merge acc  (unnest child-model v)))
                      acc))
               {table* (apply dissoc tree* join-ks)}
               tree*)))

(unnest Post p)
{:id 1,

 :title "post t",

 :body "post b",

 :published_on

 #object[java.time.LocalTime 0xccc60 "21:19:21.239712300"],

 :author {:id 1, :name "abhinav"}}

{:post {:id 1,
        :title "post t",
        :body "post b",
        :published_on
        #object[java.time.LocalTime 0xccc60 "21:19:21.239712300"],
        :author 1}
 :author {:id 1, :name "abhinav"}}



(defn update-query
  "Generates update query for `model`"
  [model value])

(defn update-models [parent-model values])

(def a {:id 1, :name "abhinav"})

(def p (Post. 1 "post t" "post b" (java.time.LocalTime/now) a))


(comment


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
                       :column? true
                       :fk :author.id}
              #_:category #_{:model Category
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
                          :mapping-table-pk :category_and_posts.category_id}}}))

(ns scribe.core
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [scribe.postgres :as pg]))

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

  ;(spec #scribe.core.a-model{:id 1})
  ;; => {:table :model, :pk :id, :record {:id #function[clojure.core/int?]}}
  )


(defn construct-record [record-sym]
  ((resolve (record-related-syms record-sym {:prefix 'map->}))
   {}))


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

(defn record
  "Extract the record of a model"
  [model]
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
  ([model tree]
   (assert (some? model) "`model` can't be nil, must be a java class.")
   (let [spec* (spec model)
         tree* (select-keys tree (keys (record model)))
         table* (:table spec*)
         join-ks (keys-for-joined-tables spec*)]
     (reduce-kv (*fn [acc k v]
                     (if (contains? join-ks k)
                       (let [child-model (-> spec* record k :model)
                             child-map (cond
                                         (map? v)
                                         (unnest child-model v)

                                 ;; if v is nil, apply will ignore it 
                                 ;; and pass only child-model to 
                                 ;; throwing an arity exception
                                         (nil? v)
                                         (unnest child-model v)

                                         (seqable? v)
                                         (apply unnest child-model v)
                                         )]
                         (merge acc child-map))
                       acc))
                {table* (apply dissoc tree* join-ks)}
                tree*)))
  ([model tree & trees]
   (->> (map (partial unnest model) (cons tree trees))
        (apply merge-with  (fn [a b]
                             (cond
                               (map? a)
                               [a b]

                               (and (not (map? a))
                                    (map? b))
                               (conj a b)

                               (and (seqable? a)
                                    (seqable? b))
                               (vec (concat a b))))))))



(defn update-query
  "Generates update query for `model`"
  [model value])

(defn update-models [parent-model values])

(comment


  (declare-model Category Post)

  (defmodel Author
    {:table :blog.author
     :pk :author.id
     :record {:id int?
              :name string?
            ;; For iteration 1 lets NOT model bidirectional relationships
              :posts {:model Post
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
              #_:author #_{:model Author
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
                          :mapping-table-pk :category_and_posts.category_id}}})
  
  
(def a2 {:id 2, :name "abhinav O" :posts [p3 p4]})

(def p (Post. 1 "post t" "post b" (java.time.LocalTime/now)))

(def p2 (Post. 2 "post 2" "post c" (java.time.LocalTime/now)))
(def p3 (Post. 3 "post 2" "post c" (java.time.LocalTime/now)))
(def p4 (Post. 4 "post 2" "post c" (java.time.LocalTime/now)))

(def a (map->Author {:id 1, :name "abhinav" :posts [p p2]}))
  )

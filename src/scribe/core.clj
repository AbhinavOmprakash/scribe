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
  "Returns the spec for model `modoel`"
  [model]
  ((resolve (record-related-syms model {:suffix '-spec}))))


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

(declare select-query)

(defn qualified-projections
  [{:keys [table pk record] :as spec}]
  (reduce-kv (fn [acc k v]
               (if (join-table? v)
                 (conj acc [(join (select-query (:model v)) pk v) k])
                 (conj acc (qaulified-column table k))))
             []
             record))


(defn select-query [model]
  (let [{:keys [table record]
         :as spec} (spec model)]
    {:select (qualified-projections spec)
     :from [table]}))

(select-query foo2)
; {:select
;  [:foo2.c
;   :foo2.d
;   [{:select [[[:to_json :child]]],
;     :from [[{:select [:foo1.a :foo1.b], :from [:foo1]} :child]],
;     :where [:= :foo2.c :a]}
;    :foo1]],
;  :from [:foo2]}


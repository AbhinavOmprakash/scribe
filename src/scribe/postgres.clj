(ns scribe.postgres)

(defn one-to-one
  [child-query parent-key foreign-key]
  {:select [[[:to_json :child]]]
   :from [[child-query :child]]
   :where [:= parent-key foreign-key]})


(defn one-to-many
  [child-query parent-key foreign-key]
  {:select [[[:json_agg :child]]]
   :from [[child-query :child]]
   :where [:= parent-key foreign-key]})


(defn many-to-many
  [{child :from :as child-table}
   parent-key
   child-key
   mapping-table
   mapping-table-parent-key
   mapping-table-child-key]
  {:select [[[:json_agg :item]]]
   :from [[{:select (vec (:select child-table))
            :from child
            :join [[mapping-table]
                   [:= mapping-table-child-key child-key]]
            :where [:= mapping-table-parent-key parent-key]}
           :item]]})

(ns familiar.db
  (:require [korma
              [core :refer :all]
              [db :refer :all]]
            [familiar.dbconfig :refer :all]))

(defn create-if-missing [this & names]
 (doall
   (for [item (remove (set (map :name
                                (select this
                                  (fields :name))))
                      names)]
     (insert this
       (values {:name item})))))

(defn get-field [field table name]
  (-> (select table
        (fields :name field)
        (where {:name name}))
      first
      field))

(defn no-concurrent-instance? [time varname]
  (empty? (select instance
            (where {:variable_id (get-field :id variable varname)
                   :time time}))))


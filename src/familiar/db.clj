(ns familiar.db
  (:require [korma
              [core :refer :all]
              [db :refer :all]]
            [clj-time
              [core :refer :all :rename {extend elongate}]
              [coerce :refer :all]
              [format :refer :all]
              [local :refer :all]]
            [familiar
              [dbconfig :refer :all]]))

(def time-form    (formatters :date-time))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(defn present []
  (unparse-time (local-now)))
(def active-time (atom (present)))

(defn get-field [field table name]
  (-> (select table
        (fields :name field)
        (where {:name name}))
      first
      field))

(defn slice [instant varname]
  (let [time-res (keyword (get-field :time-res variable varname))]
    (->> (parse-time instant)
         (unparse (formatters time-res)))))


(defn create-if-missing [this & names]
 (doall
   (for [item (remove (set (map :name
                                (select this
                                  (fields :name))))
                      names)]
     (insert this
       (values {:name item})))))

(defn value [varname time]
  (->> (select instance
         (with variable
           (fields :name))
         (where {:time (slice time varname)
                 :variable.name varname}))
       first
       :value
       read-string))

(defn no-concurrent-instance? [time varname]
  (empty? (select instance
            (where {:variable_id (get-field :id variable varname)
                    :time time}))))

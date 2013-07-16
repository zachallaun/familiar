(ns familiar.db
  (:require [korma
              [core :refer :all]
              [db :refer :all]]
            [clj-time
              [core :refer :all :rename {extend elongate}]
              [coerce :refer :all]
              [format :refer :all]
              [local :refer :all]
              [periodic :refer :all]]
            [familiar
              [dbconfig :refer :all]]))

(def time-form    (formatters :date-time))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(def date-form    (formatters :date))
(def unparse-date (partial unparse date-form))
(def parse-date   (partial parse date-form))

(defn present []
  (unparse-time (local-now)))
(def active-time (atom (present)))

(def precision-table
  {:date (days 1)})

(defn get-field [field table name]
  (-> (select table
        (fields :name field)
        (where {:name name}))
      first
      field))

(defn slice [instant varname]
  (let [time-res (keyword (get-field :time-res variable varname))]
    (unparse (formatters time-res) instant)))


(defn range-instants [start end delta-t]
  (take-while (partial within? start end)
              (iterate #(plus % delta-t)
                       start)))

(defn range-values [varname [start end]]
  (let [time-res (keyword (get-field :time-res variable varname))
        times
        (->> (range-instants start end (precision-table time-res))
             (map (partial unparse (formatters time-res))))]
    (select instance
      (fields :time :value)
      (where {:variable_id (get-field :id variable varname)
              :time        [between (map unparse-time [start end])]}))))

(defn possible-vals [variables trange]
  (map (comp set
             #(map :value %)
             #(range-values % trange))
       variables))

(defn create-if-missing [this & names]
 (doall
   (for [item (remove (set (map :name
                                (select this
                                  (fields :name))))
                      names)]
     (insert this
       (values {:name item})))))

(defn no-concurrent-instance? [time varname]
  (empty? (select instance
            (where {:variable_id (get-field :id variable varname)
                    :time time}))))

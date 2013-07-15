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

(defn range-instants [varname begin delta-t duration]
  (let [begin    (parse-time begin)
        time-res (keyword (get-field :time-res variable varname))
        final    (plus begin duration)
        slices   (take-while (partial within? begin final)
                             (iterate #(plus % delta-t)
                                      begin))]
    (set
      (map (partial unparse (formatters time-res))
           slices))))

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

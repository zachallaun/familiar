(ns familiar.time
  (:require [clj-time
               [core :refer :all :rename {extend elongate}] 
               [coerce :refer :all] 
               [format :refer :all] 
               [local :refer :all]]
            [familiar
               [db :refer :all]
               [dbconfig :refer :all]]))

(def time-form    (formatters :date-time))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(defn present []
  (unparse-time (local-now)))
(def active-time (atom (present)))

(defn slice [instant varname]
  (let [time-res (keyword (get-field :time-res variable varname))]
    (->> (parse-time instant)
         (unparse (formatters time-res)))))

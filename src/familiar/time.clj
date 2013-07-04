(ns familiar.time
  (:require [clj-time
               [core :refer :all :rename {extend elongate}] 
               [coerce :refer :all] 
               [format :refer :all] 
               [local :refer :all]]))

(defn later [a b]
  (apply > (map (comp to-long name) [a b])))
(def inst-map (sorted-map-by later))

(def time-form    (formatters :date-time))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(defn present []
  (unparse-time (local-now)))
(def active-time (atom (present)))

(defn chop-time [instant form]
  (->> (parse-time (name instant))
       (unparse (formatters form))
       keyword))

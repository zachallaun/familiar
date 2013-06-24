(defn later [a b]
    (apply > (map to-long [a b])))
(def inst-map (sorted-map-by later))

(def time-form    (formatters :date-time))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(defn present []
    (unparse-time (local-now)))
(def active-time  (atom (present)))

(defn chop-time [instant form]
  (->> instant
    (unparse (formatters form))
    (parse (formatters form))))

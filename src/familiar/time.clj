(defn later [a b]
    (apply > (map to-long [a b])))
(def inst-map (sorted-map-by later))

(def time-form    (formatters :basic-date-time-no-ms))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(defn present []
    (unparse-time (local-now)))
(def active-time  (atom (present)))
(def time-zone    (atom -5)) ;; bad

(defn set-time
  ([y m d]
    (let [m (if (> m 9) m (str "0" m))
          d (if (> d 9) d (str "0" d))
          tm (parse-time (str y m d "T000000Z"))]
      (reset! active-time (from-time-zone tm
                            (time-zone-for-offset @time-zone)))))
  ([y m d h]
    (let [m (if (> m 9) m (str "0" m))
          d (if (> d 9) d (str "0" d))
          h (if (> h 9) h (str "0" h))
          tm (parse-time (str y m d "T" h "0000Z"))]
      (reset! active-time (from-time-zone tm
                            (time-zone-for-offset @time-zone))))))

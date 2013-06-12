(defn boolean? [x]
  (or (true? x) (false? x)))

(defn num-interval [b e]
  "Returns true if a real number on interval [b,e]. Use nil
  instead of a number for infinite upper and/or lower bound."
  (fn [x]
    (try (+ x 0) (catch Exception e false))
    (cond (and (nil? b) (nil? e))
          true
          (and (nil? b) (<= x e))
          true
          (and (nil? e) (>= x b))
          true
          (and (>= x b) (<= x e))
          true
          :else false)))

(defn time? [x]
  (try
    (if (parse (formatters :hour-minute) x) true)
    (catch Exception e false)))

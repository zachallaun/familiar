(defn boolean? [x]
  (or (true? x) (false? x)))

(defn interval [b e]
  "Returns true if a real number on interval [b,e]. Enter \"inf\"
  instead of a number for infinite upper and/or lower bound."
  (fn [x]
    (try (+ x 0) (catch Exception e false))
    (cond (and (= b "inf") (= e "inf"))
          true
          (and (= b "inf") (<= x e))
          true
          (and (= e "inf") (>= x b))
          true
          (and (>= x b) (<= x e))
          true
          :else false)))

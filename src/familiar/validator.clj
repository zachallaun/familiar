(ns familiar.validator
  (:require [clj-time.format :refer [parse formatters]]))

(defn boolean? [x]
  (or (true? x) (false? x)))

(defn num-interval [b e]
  "Returns true if a real number on interval [b,e]. Use nil
  instead of a number for infinite upper and/or lower bound."
  (fn [x]
    (when (instance? java.lang.Number x)
      (or (and (nil? b) (nil? e))
          (and (nil? b) (<= x e))
          (and (nil? e) (>= x b))
          (<= b x e)))))

(defn time? [x]
  (try
    (if (parse (formatters :time) x) true)
    (catch Exception e false)))

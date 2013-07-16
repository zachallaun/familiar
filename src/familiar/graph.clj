(ns familiar.graph
  (:require [familiar
              [dbconfig :refer :all]
              [db :refer :all]]
            [clj-time
              [core :refer :all :rename {extend elongate}]
              [coerce :refer :all]
              [format :refer :all]
              [local :refer :all]]
            [korma
              [core :refer :all]
              [db :refer :all]]
            [loom
              [graph :refer :all]
              [alg :refer :all]
              [gen :refer :all]
              [attr :refer :all]
              [label :refer :all]]
            [swiss-arrows.core :refer :all]
            [clojure.math.combinatorics :refer :all]
            [clojure.pprint :refer [pprint]]))

(defn naive-skeleton
  "An annoyingly simple representation of a graph skeleton
     for an annoyingly simple machine learning model."
  [classpred featurepreds]
  (assert (nil? ((set featurepreds) classpred))
          "No cycles.")
  (digraph {classpred featurepreds}))

(defn value- [varname time]
  (-> (select instance
        (with variable
          (fields :name))
          (where {:time (slice time varname)
                  :variable.name varname}))
      first
      :value
      (as-> x
        (if x
          (read-string x)
          nil))))

(defmacro value [varname t]
  `(value- ~(str varname) ~t))

(defn calc-pred
  [pred t]
  (let [[deps pred] (map #(read-string (get-field % variable pred))
                         [:deps :fn])
        values (zipmap deps (map #(value- % t) deps))]
    (eval (clojure.walk/postwalk #(if ((set deps) (str %))
                                    (values (str %))
                                    %)
                                 pred))))

(defn times-matching
  [var-vals trange]
  (->> (select instance
         (fields :time :value)
         (with variable
           (fields :name))
         (where {:variable.name [in (keys var-vals)]
                 :time  [between (map unparse-time trange)]}))
       (partition (count var-vals))
       (filter (fn [insts]
                 (every? #(= (var-vals (:name %))
                             (:value %))
                         insts)))
       count))

(defn cond-prob-dist
  "Calculates the conditional probability distribution for variable
     and its parents in time range [:start :end], or for forever"
  [skeleton varname
   & {:keys [start end]
        :or {start (parse-date "2013-07-01")
             end   (local-now)}}]
  (let [trange       [start end]
        variables    (conj ((:in skeleton) varname) varname)
        present-vals (possible-vals variables trange)]
    (apply merge
      (for [valcoll (apply cartesian-product present-vals)]
        (let [this-vals     (zipmap variables valcoll)
              count-matches #(times-matching (select-keys this-vals %) trange)
              n-instances   (count-matches (set ((:in skeleton) varname)))]
          (hash-map this-vals
                    (/ (count-matches variables)
                       n-instances
                       1.0)))))))

(defn prior-dist
  "Calculates the prior distribution for a variable
     in time range [:start :end], or for forever"
  [varname
   & {:keys [start end]
        :or {start (parse-date "2013-07-01")
             end   (local-now)}}]
  (let [trange     [start end]
        presents   `{~varname ~@(possible-vals [varname] trange)}
        possible   (map #(hash-map varname %) (presents varname))
        total      (apply +
                          (map #(times-matching {varname %} trange)
                               (presents varname)))]
    (zipmap possible
            (map #(/ (times-matching % trange)
                     total
                     1.0)
                 possible))))

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
  (->> (select instance
         (with variable
           (fields :name))
           (where {:time (slice time varname)
                   :variable.name varname}))
       first
       :value
       read-string))

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

;; TODO does not work if no parents
(defn cond-prob-dist
  "Calculates the conditional probability distribution for variable
     and its parents in time range (+ :instant duration) given step size
     delta-t (which really shouldn't be more precise than the most
     precise variable in this CPD)"
  [skeleton varname delta-t duration
   & {:keys [instant]
        :or {instant @active-time}}]
  (let [variables   (cons varname ((:in skeleton) varname))
        start-end   [(slice instant varname)
                     (unparse-time (plus (parse-time instant) duration))]
        present-vals
          (zipmap variables
                  (map (comp set
                             #(map :value %)
                             #(range-values % instant duration))
                       variables))]
    (apply merge
      (for [valcoll (apply cartesian-product (reverse (vals present-vals)))]
        (let [this-vals   (zipmap variables valcoll)
              n-instances
                (count
                  (->> (select instance
                         (fields :time :value)
                         (with variable
                           (fields :name))
                         (where {:variable.name [in ((:in skeleton) varname)]
                                 :time [between start-end]}))
                       (partition (dec (count variables)))
                       (filter (fn [insts]
                                 (every? #(= (this-vals (:name %))
                                             (:value %))
                                         insts)))))]
          (hash-map this-vals
                    (-<>> (select instance
                            (fields :time :value)
                            (with variable
                              (fields :name))
                            (where {:variable.name [in variables]
                                    :time          [between start-end]}))
                          (partition (count variables))
                          (filter (fn [insts]
                                    (every? #(= (this-vals (:name %))
                                                (:value %))
                                            insts)))
                          count
                          (/ <> n-instances 1.0))))))))



;; example return value:
;; {{:var1 "false", :var2 "false"} 0.3
;;  {:var1 "false", :var2 "true"}  0.05
;;  {:var1 "true",  :var2 "false"} 0.55
;;  {:var1 "true",  :var2 "true"}  0.1}

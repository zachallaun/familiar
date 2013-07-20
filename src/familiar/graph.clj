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
        var-values  (zipmap deps (map #(value- % t) deps))]
    (try 
      (eval (clojure.walk/postwalk
              #(if ((set deps) (str %))
                   (var-values (str %))
                   %)
              pred))
      (catch Exception e nil))))

(defn realize-pred
  [pred] ; time range
  (let [deps (set (mapcat (comp read-string :deps)
                          (select variable
                            (where {:name pred}))))
        exist (map :time
                   (select instance
                     (where {:variable_id (get-field :id variable pred)})))]
    (doseq [t (apply sorted-set 
                (map :time (select instance
                             (fields :time)
                             (with variable
                               (fields :name))
                             (where {:variable.name [in deps]
                                     :time [not-in exist]}))))]
      (insert instance
        (values {:time t
                 :value (str (calc-pred pred (parse-date t)))
                 :variable_id (get-field :id variable pred)})))
    (delete instance
      (where {:value ""
              :variable_id (get-field :id variable pred)}))))

(defn times-matching
  [var-vals trange]
  (->> (select instance
         (fields :time :value)
         (with variable
           (fields :name))
         (where {:variable.name [in (keys var-vals)]
                 :time  [between (map unparse-time trange)]}))
       (group-by :time) ; assumes same time resolution!
       vals
       (map #(zipmap (map :name %) (map :value %)))
       (filter #(= % var-vals))
       count))

(defn cond-prob-dist
  "Calculates the conditional probability distribution for variable
     in time range [:start :end]"
  [skeleton varname
   & {:keys [start end]
        :or {start (parse-date "2013-07-01")
             end   @active-time}}]
  (let [trange        [start end]
        g-parents     ((:in skeleton) varname)
        variables     (conj g-parents varname)
        present-vals  (possible-vals variables trange)
        possibilities (apply cartesian-product present-vals)]
    (apply merge
      (for [valcoll possibilities]
        (let [this-vals     (zipmap variables valcoll)
              count-matches (times-matching this-vals trange)
              n-instances   (apply +
                                   (map #(times-matching (assoc this-vals
                                                                varname
                                                                %)
                                                         trange)
                                        (first (possible-vals [varname] trange))))]
          (hash-map this-vals
                    (/ count-matches
                       n-instances
                       1.0)))))))

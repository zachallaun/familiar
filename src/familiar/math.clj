(ns familiar.math
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

(defn naive-bayes
  "An annoyingly simple representation of a graph skeleton
     for an annoyingly simple machine learning model."
  [classpred featurepreds]
  (assert (nil? ((set featurepreds) classpred))
          "No cycles.")
  (digraph {classpred featurepreds}))

(defn value- [varname time]
  (let [[{value :value}] (select instance
                           (with variable
                             (fields :name))
                           (where {:time (slice time varname)
                                   :variable.name varname}))]
    (and x (read-string x))))

(defmacro value [varname t]
  `(value- ~(str varname) ~t))

(defn calc-pred
  [pred t]
  (let [pred (load-string (get-field :fn variable pred))]
    (pred t)))

(defn realize-pred
  [pred] ; time range
  (let [deps  (read-string (get-field :deps variable pred))
        exist (cons "aasklfjlasdjfal"
                (map :time
                  (select instance
                    (where {:variable_id (get-field :id variable pred)}))))]
    (doseq [t (apply sorted-set
                (map :time (select instance
                             (fields :time)
                             (with variable
                               (fields :name))
                             (where {:variable.name [in deps]
                                     :time [not-in exist]}))))]
      (if-not (nil? (try
                      (calc-pred pred (parse-date t))
                      (catch Exception e nil)))
        (insert instance
          (values {:time t
                   :value (str (calc-pred pred (parse-date t)))
                   :variable_id (get-field :id variable pred)}))))
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
     in time range [:start :end] according to digraph"
  [varname skeleton
   & {:keys [start end]
        :or {start (parse-date "2013-07-01")
             end   (plus @active-time (days 1))}}]
  (if (read-string (get-field :deps variable varname))
    (realize-pred varname))
  (let [trange        [start end]
        variables     (conj ((:in skeleton) varname) varname)
        present-vals  (possible-vals variables trange)
        possibilities (apply cartesian-product present-vals)]
    (apply merge
      (for [valcoll possibilities]
        (let [this-vals     (zipmap variables valcoll)
              count-matches (times-matching this-vals trange)
              denom         (apply +
                              (map #(times-matching (assoc this-vals varname %)
                                                    trange)
                                   (first (possible-vals [varname] trange))))]
          (try
            (hash-map this-vals
                      (/ count-matches
                         (double denom)))
            (catch Exception e 0)))))))

(defn prior-dist
  "Calculates the prior disribution for variable in time range [:start :end]"
  [varname]
  (cond-prob-dist (digraph varname) varname))

(defn MLE-
  [expt varname desired-val
   & {:keys [excl start end]
        :or {excl  []
             start (parse-date "2013-07-01")
             end   (plus @active-time (days 1))}}]
  (let [variables
         (->> (select experiment
                (with variable (fields :name))
                (where {:name expt}))
              (map :variable)
              first
              (map :name)
              set)
        skeleton
          (naive-bayes varname
                       (apply disj variables
                                   (conj excl varname)))
        others
          (disj variables varname)]
    (->> (for [thing others]
           (as-> (cond-prob-dist thing skeleton :start start :end end) x
                 (filter #(= desired-val ((key %) varname)) x)
                 (sort #(> (val %1) (val %2)) x)
                 (first x)
                 (hash-map (dissoc (first x) varname)
                           (double (with-precision 4 (/ (bigdec (second x))
                                                        1))))))
         (remove #(nil? (key (first %))))
         (sort #(> (val (first %1)) (val (first %2)))))))

(defmacro MLE
  "Returns a list of variables and their values most strongly correlated
     with the given variable taking on the given value."
  [expt varname desired-val
   & {:keys [excl start end]
        :or {excl  []
             start '(parse-date "2013-07-01")
             end   '(plus @active-time (days 1))}}]
  `(pprint (MLE- (str '~expt)
                 (str '~varname)
                 (str '~desired-val)
                 :excl '~(vec (map str excl))
                 :start ~start :end ~end)))

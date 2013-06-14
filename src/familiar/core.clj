(ns familiar.core
  ;(:gen-class)
   (:require [clojure.pprint :refer [pprint]]
             [clj-time
               [core :refer :all :rename {extend elongate}]
               [coerce :refer :all]
               [format :refer :all]
               [local  :refer :all]]
             [incanter
               [core :rename {extend whatever}]
               [stats :refer :all]]
             #_[clojurewerkz.titanium
               [graph :refer :all]
               [edges :refer :all]
               [vertices :refer :all]
               [types :refer :all]
               [query :refer :all]]))

(declare prn-read str->key nser experiment)

(defn later [a b]
  (apply > (map to-long [a b])))
(def inst-map (sorted-map-by later))

(def date-form    (formatters :date))
(def unparse-date (partial unparse date-form))
(def parse-date   (partial parse date-form))
(def active-date  (atom (unparse-date (local-now))))

(def time-form    (formatters :basic-date-time-no-ms))
(def unparse-time (partial unparse time-form))
(def parse-time   (partial parse time-form))
(def active-time  (atom (unparse-time (local-now))))

(load "rangefns")
(load "propfns")

(defn -main
  "Gets to know you"
  [& args]
  ;; i don't know what is this line but here it is
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar")
)

(defrecord Variable [name validator default unit instances])

(def example-experiment
  "A silly little example."
  (atom
    {:ate-salmon #familiar.core/Variable{:name "ate-salmon"
                  :validator 'boolean?
                  :default false
                  :instances inst-map
                  :unit "boolean"}
     :outside    {:name "outside"
                  :validator '(interval 0 24)
                  :default 1
                  :unit "hours"
                  :instances inst-map}
     :exercise   {:name "exercise"
                  :validator '(interval 0 4)
                  :default 0
                  :unit "subjective strenuousness"
                  :instances inst-map}
     :mood       {:name "mood"
                  :validator '(interval 1 5)
                  :default 2
                  :unit "holistic mood rating"
                  :instances inst-map}}))

(defn- add-variable-to-experiment
  [name validator default unit]
  (swap! example-experiment assoc (str->key name)
         (->Variable name 'validator default unit inst-map)))

(defmacro add-variable [name validator default unit]
  `(add-variable-to-experiment ~name '~validator ~default ~unit))

(def experiment example-experiment)

(def active-experiment-name (atom "data.txt"))

(defn set-date [y m d]
  (let [m (if (> m 9) m (str "0" m))
        d (if (> d 9) d (str "0" d))]
    (reset! active-date (str y "-" m "-" d))))

(defmacro add-variable [variable validator default unit]
  `(do (assert (~validator ~default) "Default not in range!")
       (swap! experiment
              #(assoc %
                      (str->key ~variable)
                      {:name ~variable
                       :validator (quote ~validator)
                       :default ~default
                       :unit ~unit
                       :instances inst-map}))
       (display-vars)))

(defn tag-vars [tag & variables]
  (for [v variables]
    (swap! experiment
           (fn [x]
             (update-in x
                        [(str->key v) :tags]
                        #(conj % (str->key tag)))))))

(defn add-datum  [experiment variable value]
  (assert ((eval (-> experiment ((str->key variable)) :validator)) value)
          "Value not in range!")
  (update-in experiment
             [(str->key variable) :instances]
             #(assoc % @active-date value)))

(defn add-datum! [variable value]
  (swap! experiment add-datum variable value))

(defn add-data [experiment & coll]
  (assert (even? (count coll))
          "Mismatched number of variables and values. Double check call")
  (reduce (fn [exp x] (apply add-datum exp x))
          experiment
          (partition 2 coll)))

(defn add-data! [& coll]
  (swap! experiment (fn [x] (apply add-data x))))

(defn save-experiment []
  (spit @active-experiment-name @experiment))

(defn make-experiment [title]
  (assert (= (do (spit title "" :append true)
                 (slurp title))
             "")
          "Experiment by that name already exists")
  (spit title {}))

(defn load-experiment [title]
  (assert (= @experiment (read-string (slurp @active-experiment-name)))
          "Data not yet saved!")
  (reset! active-experiment-name title)
  (reset! experiment
          (read-string (slurp title))))

(defn display-vars []
  (let [tags (->> (vals @experiment)
                  (map :tags)
                  flatten
                  set),
        keyfil #(select-keys % [:default :validator :unit :name]),
        grouped-vars (filter #(or (keyword? %) (not (empty? %)))
                     (interleave
                       (cons :no-tag tags)
                       (cons
                           (map keyfil
                                (filter #(nil? (:tags %))
                                        (vals @experiment)))
                           (for [tag tags]
                               (map keyfil
                                    (filter #(some (partial = tag) (:tags %))
                                            (vals @experiment)))))))]
    (pprint grouped-vars)))

(defn missing-today []
  (map :name (remove (fn [m]
                       (->> (:instances m)
                            (ffirst)
                            (= @active-data)))
                     (vals @experiment))))

(defn let-default [& variables]
  (->> (vals @experiment)
       (filter #((set variables) (:name %)))
       (map :default)
       (interleave variables)
       (apply add-data)))

;~~~~ Helpers ~~~~

(defn prn-read [p]
  (println p)
  (read))

(defn str->key [s]
  (->> (str s)
       (replace {\space \-})
       (apply str) .toLowerCase
       keyword))

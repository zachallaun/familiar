(ns familiar.core
  ;(:gen-class)
   (:require [clojure.pprint :refer [pprint]]
             [swiss-arrows.core :refer :all]
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

(declare prn-read str->key active-expt display-vars)

(load "time")
(load "rangefns")
(load "propfns")

#_(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar"))

(def example-experiment
  "A silly little example."
  (atom
    {:ate-salmon {:name "ate-salmon"
                  :validator 'boolean? 
                  :default false
                  :instances inst-map 
                  :unit "boolean"
                  :time-res :day
                  :tags '(:food)}
     :outside    {:name "outside"
                  :validator '(num-interval 0 24) 
                  :default 1 
                  :unit "hours"
                  :instances inst-map
                  :time-res :day
                  :tags '()}
     :exercise   {:name "exercise"
                  :validator #{0 1 2 3} 
                  :default 0
                  :unit "subjective strenuousness"
                  :instances inst-map
                  :time-res :day
                  :tags '(:fitness)}
     :mood       {:name "mood"
                  :validator #{1 2 3 4 5}
                  :default 2
                  :unit "holistic mood rating"
                  :instances inst-map
                  :time-res :day
                  :tags '()}}))

(def active-expt example-experiment)

(def active-experiment-name (atom "data.txt"))

(defmacro add-var
  "Adds variable to experiment.
     Optional arguments:
     :expt - name of a loaded experiment (defaults to active experiment)
     :time-res - time resolution (defaults to :day)
     :unit - a string representing the unit of measure
     :tags - a sequence of strings with which to tag the variable"
  [variable validator default & opts]
  `(add-variable- ~variable '~validator ~default ~@opts))

(defn- add-variable-
  [variable validator default
   & {:keys [expt time-res unit tags]
        :or {expt active-expt
             time-res :day
             unit ""
             tags '()}}]
  (let [varkey (str->key variable)
        valfn (eval validator)
        tags (map str->key tags)]
    (assert (nil? (varkey @expt)) "Variable already exists") 
    (assert (valfn default) "Default not in range")
    (swap! expt 
           assoc   
           varkey
           {:name variable
            :validator validator
            :default default
            :unit unit
            :instances inst-map
            :time-res (keyword time-res)
            :tags tags})
    (display-vars)))

;;;;
(defn tag-var
  "Adds tag to given variables in the current experiment."
  [tag & variables]
  (for [v variables]
    (swap! experiment
           update-in 
           [(str->key v) :tags]
           #(conj % (str->key tag)))))

;;;; assert no data within current time increment
(defn add-datum
  "Adds a single instance of variable."
  [variable value & {:keys [expt instant]
                     :or {expt active-expt
                          instant @active-time}}]
  (assert ((eval (-> @expt 
                     ((str->key variable))
                     :validator))
           value)
          "Value not in range")
  (swap! expt 
         update-in 
         [(str->key variable) :instances] 
         #(assoc % instant value)))

;;;;
(defn add-data 
  "Adds instances of variables with values at active time.
     Example:
     (add-data \"mice\" 6 \"cats\" 2 \"dogs\" 0)"
  [& coll]
  (assert (even? (count coll))
          "Mismatched number of variables and values. Double check call")
  (doall (map (partial apply add-datum)
              (partition 2 coll)))
  (display-vars))

(defn save-experiment 
  "Saves the active experiment to its source file,
     or to a new file if given."
  ([]
    (spit @active-experiment-name @active-expt))
  ([title]
    (reset! active-experiment-name title)
    (spit title @active-expt)))

(defn make-experiment
  "Creates a new experiment with given filename."
  [title]
  (assert (= (do (spit title "" :append true)
                 (slurp title))
             "")
          "Experiment by that name already exists")
  (spit title {}))

(defn load-experiment 
  "Loads experiment from file, ensuring current experiment has been saved."
  [title]
  (assert (= @active-expt (read-string (slurp @active-experiment-name)))
          "Data not yet saved!")
  (reset! active-experiment-name title)
  (reset! active-expt  
          (read-string (slurp title))))

(defn display-vars
  "Displays info for variables in active experiment, grouped by tags." 
  []
  (let [tags (->> (vals @active-expt)
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

(defn missing-today
  "Displays all variables with no instance for the active date."
  []
  (map :name (remove (fn [m]
                       (->> (:instances m)
                            (#(= (ffirst %) @active-time))))
                     (vals @active-expt))))

(defn let-default 
  "Allows specified variables to take on their default values."
  [& variables]
  (->> (vals @active-expt)
       (filter #((set variables) (:name %)))
       (map :default)
       (interleave variables)
       (apply add-data)))

(defn help []
  (->> (for [[n v] (ns-publics 'familiar.core)]
         [(str "- " n) "\n    " (:doc (meta v))])
       (remove #(nil? (nth % 2)))
       (interpose "\n")
       flatten
       println))

;~~~~ Helpers ~~~~

(defn- prn-read [p]
  (println p)
  (read))

(defn- str->key [s] 
  (->> (str s)
       (replace {\space \-})
       (apply str) 
       .toLowerCase
       keyword))

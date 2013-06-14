(ns familiar.core
  ;(:gen-class)
   (:require [clojure.pprint :refer [pprint]]
             [clojure.repl :refer [doc]]
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

(declare prn-read str->key experiment display-vars)

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

(def example-experiment
  "A silly little example."
  (atom
    {:ate-salmon {:name "ate-salmon"
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

(def experiment example-experiment)

(def active-experiment-name (atom "data.txt"))

(defn set-date [y m d]
  (let [m (if (> m 9) m (str "0" m))
        d (if (> d 9) d (str "0" d))]
    (reset! active-date (str y "-" m "-" d))))

(defmacro add-variable
  "Adds variable to current experiment."
  [variable validator default unit]
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

(defn tag-vars
  "Adds tag to given variables in the current experiment."
  [tag & variables]
  (for [v variables]
    (swap! experiment
           (fn [x]
             (update-in x
                        [(str->key v) :tags]
                        #(conj % (str->key tag)))))))

(defn add-datum
  "Adds a single instance of variable at active date."
  [variable value]
  (assert ((eval (-> @experiment ((str->key variable)) :validator)) value)
          "Value not in range!")
  (swap! experiment (fn [x]
                     (update-in x 
                                [(str->key variable) :instances] 
                                #(assoc % @active-date value)))))

(defn add-data 
  "Adds instances of variables with values at active date.
     Example:
     (add-data \"mice\" 6 \"cats\" 2 \"dogs\" 0)"
  [& coll]
  (assert (even? (count coll))
          "Mismatched number of variables and values. Double check call")
  (map (partial apply add-datum) (partition 2 coll))
  (display-vars))

(defn save-experiment 
  "Saves the current experiment to source file for current experiment,
     or to a new file if given."
  [& title]
  (if (first title)
    (spit (first title) @experiment)
    (spit @active-experiment-name @experiment)))

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
  (assert (= @experiment (read-string (slurp @active-experiment-name)))
          "Data not yet saved!")
  (reset! active-experiment-name title)
  (reset! experiment  
          (read-string (slurp title))))

(defn display-vars
  "Displays info for variables in active experiment, grouped by tags." 
  []
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

(defn missing-today
  "Displays all variables with no instance for the active date."
  []
  (map :name (remove (fn [m]
                       (->> m
                            :instances
                            (#(= (ffirst %) @active-date))))
                     (vals @experiment))))

(defn let-default 
  "Allows specified variables to take on their default values."
  [& variables]
  (->> (vals @experiment)
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
       (apply str) .toLowerCase
       keyword))

(ns familiar.core
  ;(:gen-class)
   (:require [clj-time
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

(def time-form    :hour-minute)
(def unparse-time #(format-local-time % time-form))
(def parse-time   (partial parse (formatters time-form)))
(def active-time  (atom (unparse-time (local-now))))

(load "rangefns")
(load "propfns")

(defn -main
  "Gets to know you"
  [& args]
  ;; i don't know what is this line but here it is
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar")
  ;; enter+save data
  ;; view+update statistics
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

(defmacro add-variable [variable validator default unit]
  `(do (assert (~validator ~default) "Default not in range!")
       (swap! experiment #(assoc %  
                                 (str->key ~variable)
                                 {:name ~variable
                                  :validator (quote ~validator)
                                  :default ~default
                                  :unit ~unit
                                  :instances inst-map}))))

(defn add-datum [variable value]
  (assert ((eval (-> @experiment ((str->key variable)) :validator)) value)
          "Value not in range!")
  (swap! experiment (fn [v]
                     (update-in v 
                                [(str->key variable) :instances] 
                                #(assoc % @active-date value)))))

(defn add-data [& coll]
  (map (partial apply add-datum) (partition 2 coll)))

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
  (println (interpose "\n" (partition 5 
                             (interleave (keys @experiment) 
                             (repeat "\t")
                             (map :unit (vals @experiment))
                             (repeat "\t")
                             (map :validator (vals @experiment)))))))

(defn missing-today []
  (map :name (remove (fn [m]
                       (->> m
                            :instances
                            (#(= (ffirst %) @active-date))))
                     (vals @experiment))))

(defn let-default [& variables]
  (->> (vals @experiment)
       (filter #((set variables) (:name %)))
       (map :default)
       (interleave variables)
       (apply add-data)))

;~~~~ Helpers ~~~~

(defn prn-read [p] (do (println p)
                       (read)))

(defn str->key [s] (->> s
                        str
                        (replace {\space \-})
                        (apply str) .toLowerCase
                        keyword))

(defmacro nser [form ns-]
  "Puts first symbol in form in given namespace"
  `(list (symbol (str (quote ~ns-)
                      "/"
                      (quote ~(first form))))
         ~@(rest form)))







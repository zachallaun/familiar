(ns familiar.core
  ;(:gen-class)
   (:require [clj-time.core :as jtime]
             [clj-time.coerce :as jtimec]
             [clj-time.format :as jtimef]
             [clojurewerkz.titanium.graph    :as gr]
             [clojurewerkz.titanium.edges    :as gre]
             [clojurewerkz.titanium.vertices :as grv]
             [clojurewerkz.titanium.types    :as grt]
             [clojurewerkz.titanium.query    :as grq]))

(declare prn-read str->key)

(load "rangefns")

(defn -main
  "Gets to know you"
  [& args]
  ;; i don't know what is this line but here it is
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar")
  ;; enter+save data
  ;; view+update statistics
)

(defn later [a b]
  (apply > (map jtimec/to-long [a b])))
(def inst-map (sorted-map-by later))

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
                  :default :ask
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

(def time-form    (jtimef/formatters :date))
(def unparse-time (partial jtimef/unparse time-form))
(def parse-time   (partial jtimef/parse time-form))
(def active-time  (atom (unparse-time (jtime/now))))

(defn set-date [y m d]
  (let [m (if (> m 9) m (str "0" m))
        d (if (> d 9) d (str "0" d))]
    (reset! active-time (str y "-" m "-" d))))

(defmacro add-variable [variable validator default unit]
  `(do (assert (~validator ~default) "Default not in range!")
       (swap! experiment #(assoc %  
                                 (str->key ~variable)
                                 {:name ~variable
                                  :validator (quote ~validator)
                                  :default ~default
                                  :unit ~unit
                                  :instances inst-map}))))

;; dubiously useful
(defn add-variable-guided []
  (let [variable   (prn-read "Enter variable name")
        validator  (prn-read "Enter range form")
        default    (prn-read "Enter default value")
        unit       (prn-read "Enter unit")]
    (eval `(add-variable ~(str variable) ~validator ~default ~(str unit)))))

(defn add-datum [variable value]
  (assert ((eval (-> @experiment ((str->key variable)) :validator)) value)
          "Value not in range!")
  (swap! experiment (fn [v]
                     (update-in v 
                                [(str->key variable) :instances] 
                                #(assoc % @active-time value)))))

(defn add-data [& coll]
  (map (partial apply add-datum) (partition 2 coll)))

;; dubiously useful
(defn add-data-guided []
  (loop [coll []]
    (let [variable (prn-read "Enter variable name or nil to quit")
          value    (if variable
                      (prn-read "Enter value or nil to quit")
                       nil)]
      (if (some #(= nil %) [variable value])
        (recur (conj coll (str->key variable) value))
        (apply add-data coll)))))

(defn save-experiment []
  (spit @active-experiment-name @experiment))

(defn make-experiment [title]
  (assert (= (do (spit title "" :append true)
                 (slurp title))
             "")
          "Experiment by that name already exists")
  (spit title {}))

(defn load-experiment [title]
  #_(assert (= @experiment (read-string (slurp @active-experiment-name)))
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
                            (#(= (ffirst %) @active-time))))
                     (vals @experiment))))

;fn to use defaults on today's missing variables

;~~~~ Helpers ~~~~

(defn prn-read [p] (do (println p)
                       (read)))

(defn str->key [s] (->> s
                        str
                        (replace {\space \-})
                        (apply str) .toLowerCase
                        keyword))

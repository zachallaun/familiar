(ns familiar.core
  ;(:gen-class)
   (:require [clj-time.core :as jtime]
             [clj-time.format :as jtimef]))

(declare prn-read str->key)

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
  (atom    
    {:ate-salmon {:range-form '(fn [n] (or (false? n) (true? n)))
                  :default false
                  :instances { }
                  :unit "boolean"}
     :outside    {:range-form '(fn [n] (>= n 0)) 
                  :default :ask
                  :unit "hours"
                  :instances { }}
     :exercise   {:range-form (set (range 3))
                  :default 0
                  :unit "subjective strenuousness"
                  :instances { }}
     :mood       {:range-form (set (range 5))
                  :default 2
                  :unit "holistic mood rating"
                  :instances { }}}))

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

(defn add-variable [variable range-form default unit]
  (assert ((eval range-form) default) "Default not in range!")
  (swap! experiment #(assoc % (str->key variable)
                              {:range-form range-form
                               :default default
                               :unit unit
                               :instances {}})))

(defn add-variable-guided []
  (let [variable   (prn-read "Enter variable name")
        range-form (prn-read "Enter range form")
        default    (prn-read "Enter default value")
        unit       (prn-read "Enter unit")]
    (add-variable (str variable) range-form default (str unit))))

(defn add-datum [variable value]
  (assert ((eval (-> @experiment ((str->key variable)) :range-form)) value)
          "Value not in range!")
  (swap! experiment (fn [v]
                     (update-in v 
                                [(str->key variable) :instances] 
                                #(assoc % @active-time value)))))

(defn add-data [& coll]
  (map (partial apply add-datum) (partition 2 coll)))

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
                             (map :range-form (vals @experiment))
                             (repeat "\t")
                             (map :unit (vals @experiment)))))))

(defn missing-today []
  (println (interleave (->> @experiment
                            (remove #(-> % :instances @active-time))
                            keys)
                       (repeat "\n"))))

;~~~~ Helpers ~~~~

(defn prn-read [p] (do (println p)
                       (read)))

(defn str->key [s] (->> s
                       str
                       (replace {\space \-})
                       (apply str)
                       .toLowerCase
                       keyword))

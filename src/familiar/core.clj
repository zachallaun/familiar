(ns familiar.core
  ;(:gen-class)
   (:require [clojure.pprint :refer [pprint]]
             [clojure.tools.namespace.repl :refer [refresh]]
             [swiss-arrows.core :refer :all]
             [clj-time
               [core :refer :all :rename {extend elongate}] 
               [coerce :refer :all] 
               [format :refer :all] 
               [local :refer :all]]))

;; forward declarations required, just like in history class! cool!
(declare str->key active-expt display-vars)

(load "time")
(load "rangefns")
(load "propfns")

(def example-experiment
  "A silly little example."
  (atom
    {:ate-salmon {:name "ate-salmon"
                  :validator 'boolean? 
                  :default false
                  :instances inst-map 
                  :unit "boolean"
                  :time-res :date
                  :tags '(:food)}
     :outside    {:name "outside"
                  :validator '(num-interval 0 24) 
                  :default 1 
                  :unit "hours"
                  :instances inst-map
                  :time-res :date
                  :tags '()}
     :exercise   {:name "exercise"
                  :validator #{0 1 2 3} 
                  :default 0
                  :unit "subjective strenuousness"
                  :instances inst-map
                  :time-res :date
                  :tags '(:fitness)}
     :mood       {:name "mood"
                  :validator #{1 2 3 4 5}
                  :default 2
                  :unit "holistic mood rating"
                  :instances inst-map
                  :time-res :date
                  :tags '()}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;~~~~ Experiment management ~~~~

(def active-expt example-experiment)

(def active-expt-name (atom "data"))

(defn save-expt 
  "Saves an experiment to a file, defaulting to 
     active experiment and its source file.
     Keyword arguments:
     :expt - name of experiment
     :file - file to write to"
  [& {:keys [expt file]
        :or {expt active-expt
             file @active-expt-name}}]
  (spit file @active-expt))

(defn make-expt
  "Puts a blank experiment into a new file of given name."
  [file]
  (assert (= (do (spit file "" :append true)
                 (slurp file))
             "")
          "Experiment by that name already exists")
  (spit file {}))

(defn load-expt
  "Loads experiment from file, ensuring current 
     experiment has been saved to its source."
  [file]
  (assert (= @active-expt (read-string (slurp @active-expt-name)))
          "Data not yet saved!")
  (reset! active-expt-name file)
  (reset! active-expt
          (read-string (slurp file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;~~~~~ Variables and Data ~~~~

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
             time-res :date
             unit ""
             tags '()}}]
  (let [varkey (str->key variable)
        tags (map str->key tags)]
    (assert (nil? (varkey @expt)) "Variable already exists") 
    (assert ((eval validator) default) "Default fails validator")
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

(defn add-datum
  "Adds a single instance of variable."
  [varname value & {:keys [expt instant]
                      :or {expt active-expt
                           instant @active-time}}]
  (let [variable ((str->key varname) @expt)
        instant (chop-time instant (:time-res variable))]
    (assert ((eval (:validator variable)) value)
            "Value fails validator")
    (swap! expt 
           update-in 
           [(str->key varname) :instances] 
           #(assoc % instant value))))

(defn add-data 
  "Adds instances of variables with values.
     Example:
     (add-data [\"mice\" 6 \"cats\" 2 \"dogs\" 0])"
  [coll & {:keys [expt instant]
             :or {expt active-expt
                  instant @active-time}}]
  (assert (even? (count coll))
          "Mismatched number of variables and values. Double check call")
  (doall (map (partial apply add-datum)
              (map #(conj (vec %) :expt expt :instant instant)
                   (partition 2 coll))))
  (display-vars))

;;;;
(defn tag-vars
  "Adds tag to each var in collection in an experiment."
  [tag vars & {:keys [expt]
                 :or {expt active-expt}}]
  (for [v vars]
    (swap! expt 
           update-in 
           [(str->key v) :tags]
           #(conj % (str->key tag)))))

;; desperately needs rewriting, good lord
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
                                        (vals @active-expt)))
                           (for [tag tags]
                               (map keyfil
                                    (filter #(some (partial = tag) (:tags %))
                                            (vals @active-expt)))))))]
    (pprint grouped-vars)))

;;;;
(defn missing-today
  "Displays all variables with no instance for their
     time pixel overlapping the active time."
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

(defn- str->key [s] 
  (->> (str s)
       (replace {\space \-})
       (apply str) 
       .toLowerCase
       keyword))

#_(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar"))

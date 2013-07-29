(ns familiar.core
  (:gen-class :main true)
  (:require [familiar
              [dbconfig :refer :all]
              [db :refer :all]
              [validator :refer :all]
              [math :refer :all]]
            [korma
              [core :refer :all]
              [db :refer :all]]
            [lobos
              [connectivity :as lc]
              [core :as l]
              [schema :as ls]]
            [clojure
              [walk :as walk]
              [pprint :refer [pprint]]
              [repl :refer [doc source]]]
            [clojure.java.jdbc :as jdb]
            [clojure.java.jdbc.sql :as sql]
            [swiss-arrows.core :refer :all]
            [clj-time
              [core :refer :all :rename {extend elongate}]
              [coerce :refer :all]
              [format :refer :all]
              [local :refer :all]]
            [loom
              [graph :refer :all]
              [alg :refer :all]
              [gen :refer :all]
              [attr :refer :all]
              [label :refer :all]
              [io :refer :all]]))

(declare str->key with-str-args display-vars active-expt)

;;;;;;;;;;;;;;;
;; Preferences
;;

(if (not (.exists (clojure.java.io/file "preferences")))
  (spit "preferences" {:default "default"}))

(def preferences (atom (read-string (slurp "preferences"))))

(defn pref-
  [title thingy]
  (swap! preferences (assoc title thingy))
  (spit "preferences" @preferences))

(defmacro pref
  "Sets a setting to a value."
  [title thingy]
  `(pref- (keyword ~title) (str ~thingy)))

(defn prefs
  "Displays your current preferences."
  []
  (pprint @preferences))

;;;;;;;;;;;;;;;
;; Databases
;;

(def db (atom (h2 {:db "data/default/default.db"})))

(defn open-
  [file]
  (reset! db (h2 {:db (str "data/" file  "/" file ".db")}))
  (default-connection (create-db @db))
  (create-tables @db))

(defmacro open!
  "Changes active experiment to that with the specified name"
  [file]
  (open- (str file)))

(open- (:default @preferences))

;;;;;;;;;;;;;
;; Variables
;;

(defn- tag-var-
  [varname & tags]
  (assert (seq (get-field :name variable varname))
          (str "No variable by the name " varname "."))
  (apply insert-if-missing tag tags)
  (for [item tags]
    (insert variable_tag
      (values {:tag_id      (get-field :id tag item)
               :variable_id (get-field :id variable varname)})))
  (println (str varname " tagged: " tags)))

(defmacro tag-var
  "Adds tags to variable
     Example: (tag-var aye-aye primate terrifying nocturnal)"
  [& args]
  `(tag-var- ~@(map str args)))

(defn- new-var-
  [[varname validator default]
   & {:keys [time-res unit tags] :or {time-res "date" unit "" tags "()"}}]
  (assert ((eval (read-string validator)) (read-string default))
          "Given default fails validator.")
  (assert (not (nil? (read-string default)))
          "nil default is reserved for predicates. Deal with it.")
  (insert variable
    (values {:name varname, :default default, :unit unit,
             :time-res time-res, :fn validator, :deps "nil"}))
  (apply tag-var- varname (read-string tags)))

(defmacro new-var
  "Adds variable to experiment.
     Example: (new-var robot boolean? false)
     Optional arguments:
     :time-res - time resolution (defaults to date, can be date-time)
     :unit - a string representing the unit of measure
     :tags - a sequence of strings with which to tag the variable"
  [& exprs]
  `(with-str-args new-var- ~exprs))

(defn get-deps [predicate]
  (set
    (filter (set (map :name
                      (select variable (fields :name))))
            (map str (flatten (read-string predicate))))))

(defn- new-pred-
  [coll & {:keys [time-res unit tags] :or {time-res "date" unit "" tags "()"}}]
  (let [[predname function] coll]
    (insert variable
      (values {:name predname, :default "nil", :unit unit, :time-res time-res,
               :fn function, :deps (str (get-deps function))}))
    (apply tag-var- predname (conj (read-string tags)
                                   "predicate"))))

(defmacro new-pred
  "Adds predicate to experiment.
     Example:
     (new-pred accomplished
               #(>= (value productivity %) 3)
     Accepts same optional arguments as new-var (:time-res, :unit, :tags)"
  [& exprs]
  `(with-str-args new-pred- ~exprs))

(defn- display-
  [tags]
  (let [tags (if (seq tags)
               (partial some (set (map str tags)))
               (constantly true))]
    (->> (select variable
           (with tag)
           (where {:default [not= "nil"]}))
         (map (fn [t] (update-in t [:tag]
                                 #(map :name %))))
         (filter #(tags (:tag %)))
         (map #(select-keys % [:default :fn :unit :tag :name]))
         pprint)))

(defmacro display
  "Displays info for variables in active experiment that match tags, or all
     variables if no arguments."
  [& tags]
  (display- (map str tags)))

(defn- validate [varname value]
  (let [validator (-> (get-field :fn variable varname)
                      read-string
                      eval)]
    (validator value)))

;;;;;;;;
;; Data
;;

(defn datum
  "Adds a single instance of variable."
  [varname value & {:keys [instant] :or {instant @active-time}}]
  (let [timeslice (slice instant varname)]
    (assert (not (nil? (get-field :default variable varname)))
            "Cannot add data for predicates")
    (assert (validate varname (read-string value))
            (str value " is invalid for " varname
                 "\nValidator: " (get-field :fn variable varname)))
    (assert (no-concurrent-instance? timeslice varname)
            (str varname " already has value at " timeslice))
    (insert instance
      (values {:time timeslice
               :value value
               :variable_id (get-field :id variable varname)}))
    (str varname ": " value)))

(defn- data-
  [coll & {:keys [instant] :or {instant @active-time}}]
  (transaction
    (try (doall
      (->> (partition 2 coll)
           (map #(concat % [:instant instant]))
           (map #(apply datum %))))
      (catch Throwable e (println (.getMessage e))
                         (rollback)))))

(defmacro data
  "Adds instances of variables with values.
     Example:
     (data marmots 3 lorises 7 capuchins 0 joy true)"
  [& exprs]
  `(with-str-args data- ~exprs))

(defn- erase-
  [coll & {:keys [instant] :or {instant @active-time}}]
  (let [slices
          (map (comp (partial slice instant) :name)
               (select variable (fields :name) (where {:name [in coll]})))
        ids
          (map :id
               (select variable (fields :id) (where {:name [in coll]})))]
    (transaction
      (delete instance
        (fields :time :variable_id)
        (where {:time [in (set slices)]
                :variable_id [in ids]})))))

(defmacro erase
  "Erases data for given variables at active time/given time."
  [& exprs]
  `(with-str-args erase- ~exprs))

(defn missing-
  [& {:keys [instant] :or {instant @active-time}}]
  (->> (select variable
         (fields :default :fn :name)
         (where {:default [not= "nil"]}))
       (map vals)
       (filter #(no-concurrent-instance? (slice instant
                                                (first %))
                                         (first %)))))

(defn missing
  "Displays all variables with no instance for the
     time pixel matching the active time/given time."
  [& {:keys [instant] :or {instant @active-time}}]
  (->> (missing- :instant instant)
       (interpose "\n")
       println))

(defn entered
  "Displays values for variables with an instance within
     the time pixel matching the active time/given time."
  [& {:keys [instant] :or {instant @active-time}}]
  (as-> (select variable
          (fields :name)
          (where {:default [not= "nil"]})) x
        (map :name x)
        (remove (set (map first (missing- :instant instant))) x)
        (zipmap x (map #(value- % instant) x))))


(defn- defaults-
  [variables & {:keys [instant]
                  :or {instant @active-time}}]
  (-<>> (select variable
          (fields :name :default)
          (where {:name [in variables]
                  :default [not= "nil"]})
          (order :name))
        (map :default)
        (interleave (sort variables))
        (data- <> :instant instant)))

(defmacro defaults
  "Allows given variables to take on their default values"
  [& exprs]
  `(with-str-args defaults- ~exprs))

(defn change-time
  "Changes active time by specified interval.
     Example: (change-time (days -1) (hours -6))"
  [& interval]
  (swap! active-time
         #(apply plus % interval))
  (readable-time @active-time))

(defn datagen
  "Generates data for every delta-t in a variable from instant
     to (plus instant duration) according to func."
  [varname func delta-t duration
   & {:keys [instant] :or {instant @active-time}}]
  (let [[start end] (sort [instant (plus instant duration)])
        instants    (range-instants start end delta-t)]
    (doall
      (map #(datum varname (str (func)) :instant %)
           instants))))

;;;;;;;;;;;;;;;
;; Experiments
;;

(defn new-expt-
  [title variables]
  (insert experiment
    (values {:name title}))
  (let [expt-id (get-field :id experiment title)]
    (for [thing variables]
      (insert variable_experiment
        (values {:variable_id (get-field :id variable thing)
                 :experiment_id expt-id})))))

(defmacro new-expt
  "Defines an experiment on a set of variables/predicates."
  [title & variables]
  `(new-expt- (str '~title) (map str '~variables)))

;;;;;;;;;
;; Using
;;

(def valid-fns
  {:variable  #{'new-var 'tag-var 'display 'new-pred}
   :data      #{'data 'erase 'entered 'missing 'defaults 'change-time}
   :inference #{'correlations}
   :familiar  #{'open! 'doc 'pref 'prefs}
   :etc       #{'help}})

(defn cl-loop []
  (println "\n///")
  (let [input (read-line)
        input (if (and (= \( (first input))
                       (= \) (last input)))
                (read-string input)
                (read-string (str \( input \))))]
    (println "\\\\\\")
    (try
      (cond
        (#{'(quit) '(exit)} input)
        (do (println "Hooray! See you later.")
            (System/exit 0))

        (#{'sudo} (first input))
        (pprint (eval (second input)))

        ((set (apply concat (vals valid-fns))) (first input))
        (pprint (eval input))

        :else
        (println (str "That is not allowed here. Start a Clojure REPL "
                      "if you want to think outside the box")))
      (catch Exception e
        (println (str "That didn't work.\n" (.getMessage e)))))
    (recur)))

(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (binding [*ns* (the-ns 'familiar.core)]
    (cond
      (seq args)
      (do (println args)
          (println
            (->> (map read-string args)
                 (map eval)
                 doall)))

      :else
      (do (println (str "\nFamiliar - Quantified Reasoning Assistant"
                        "\nThe active time is " (readable-time @active-time)
                        "\nFor help type \"help\""))
          (cl-loop)))))

;;;;;;;;;;;
;; Helpers
;;

(defn help-
  [domain]
  (let [domain (if domain
                 (domain valid-fns)
                 (hash-set 'help))]
    (->> (for [[n v] (filter #(domain (first %))
                             (ns-map 'familiar.core))]
           [(str "  " n) "\n    " (:doc (meta v))])
         (remove #(nil? (nth % 2)))
         (interpose "\n")
         flatten
         println)))

(defmacro help
  "Informs you how to do things, e.g. (help data)
     Valid arguments:
     variable  - functions for creating, tagging, and inspecting variables
     data      - functions for entering and inspecting data
     inference - functions for learning from your data
     familiar  - none of the above
     (quit by typing exit or quit)"
  [& domain]
  (help- (keyword (first domain))))

(defn- str->key [s]
  (->> (str s)
       (replace {\space \-})
       (apply str)
       .toLowerCase
       keyword))

(defmacro with-str-args [f exprs]
  (let [[args opts] (split-with (complement keyword?)
                                exprs)
        args (map str args)
        opts (map #(if (keyword? %) % (str %)) opts)]
    `(~f '~args ~@opts)))

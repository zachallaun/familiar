(ns familiar.core
  ;(:gen-class)
   (:require [clojure.pprint :refer [pprint]]
             [familiar
              [dbconfig :refer :all]
              [db :refer :all]
              [time :refer :all]
              [validator :refer :all]]
             [korma
              [core :refer :all]
              [db :refer :all]]
             [lobos
              [connectivity :as lc]
              [core :as l]
              [schema :as ls]]
             [clojure.java.jdbc :as jdb]
             [clojure.java.jdbc.sql :as sql]
             [swiss-arrows.core :refer :all]
             [clj-time
               [core :refer :all :rename {extend elongate}] 
               [coerce :refer :all] 
               [format :refer :all] 
               [local :refer :all]]))

(declare str->key with-str-args active-expt display-vars active-expt active-expt-name)

;;;;;;;;;;;;;;;
;; Experiments
;;

(def db (atom (h2 {:db "data/default/default.db"})))
(def korma-db (atom (create-db @db)))

(defn open!
  "Changes active experiment to that with the specified name"
  [file]
  (reset! db (h2 {:db (str "data/" file  "/" file ".db")}))
  (reset! korma-db (create-db @db))
  (default-connection @korma-db)
  (create-tables @db))

(open! "default")

;;;;;;;;;;;;;
;; Variables
;;

(defn- tag-var-
  [varname & tags]
  (assert (seq (get-field :name variable varname))
          (str "No variable by the name " varname "."))
  (apply create-if-missing tag tags)
  (for [name tags]
    (insert variable_tag
      (values {:tag_id      (get-field :id tag name)
               :variable_id (get-field :id variable varname)}))))

(defmacro tag-var
  "Adds tags to variable"
  [& args]
  `(tag-var- ~@(map str args)))

(defn- new-var-
  [[name validator default]
   & {:keys [expt time-res unit tags]
        :or {expt active-expt
             time-res "date"
             unit ""
             tags '()}}]
  (assert ((eval (read-string validator)) (read-string default))
          "Given default fails validator.")
  (insert variable
    (values {:name name
             :default default
             :unit unit
             :time-res time-res
             :validator validator}))
  (apply tag-var- name tags))

(defmacro new-var
  "Adds variable to experiment.
     Example: (new-var robot boolean? false)
     Optional arguments:
     :expt - name of an experiment (defaults to loaded experiment)
     :time-res - time resolution (defaults to date, can be date-time)
     :unit - a string representing the unit of measure
     :tags - a sequence of strings with which to tag the variable"
  [& exprs]
  `(with-str-args new-var- ~exprs))

(defn display
  "Displays info for variables in active experiment."
  []
  (->> (select variable (with tag))
       (map (comp
              #(select-keys % [:default :validator :unit :tag :name])
              (fn [t]
                (update-in t [:tag] #(map :name %)))))
       pprint))

(defn- validate [varname value]
  (let [validator (-> (get-field :validator variable varname)
                      read-string
                      eval)]
    (validator value)))

;;;;;;;;
;; Data
;;

(defn datum
  "Adds a single instance of variable."
  [varname value & {:keys [expt instant]
                      :or {expt active-expt, instant @active-time}}]
  (let [timeslice (slice instant varname)]
    (assert (validate varname (read-string value))
            (str value " is invalid for " varname))
    (assert (no-concurrent-instance? timeslice varname)
            (str varname " already has value at " timeslice))
    (insert instance
      (values {:time timeslice
               :value value
               :variable_id (get-field :id variable varname)}))))

(defn- data-
  [coll & {:keys [expt instant]
             :or {expt active-expt, instant @active-time}}]
  (transaction
    (try (doall
      (->> (partition 2 coll)
           (map #(concat % [:expt expt :instant instant]))
           (map #(apply datum %))))
      (catch Throwable e (println (.getMessage e))
                         (rollback)))))

(defmacro data
  "Adds instances of variables with values.
     Example:
     (add-data mice 6 cats 2 dogs 0)"
  [& exprs]
  `(with-str-args data- ~exprs))

(defn missing
  "Displays all variables with no instance for the
     time pixel matching the active time/given time."
  [& {:keys [expt instant]
        :or {expt active-expt, instant @active-time}}]
  (->> (map :name (select variable (fields :name)))
       (filter #(no-concurrent-instance? (slice instant %) %))))

(defn entered
  "Displays values for variables with an instance within
     the time pixel matching the active time/given time."
  [& {:keys [expt instant]
        :or {expt active-expt, instant @active-time}}]
  (remove (set (missing :expt expt :instant instant))
          (map :name (select variable (fields :name)))))

(defn- defaults-
  [variables & {:keys [expt instant]
                  :or {expt active-expt, instant @active-time}}]
  (-<>> (select variable
          (fields :name :default)
          (where {:name [in variables]})
          (order :name))
        (map :default)
        (interleave (sort variables))
        (data- <> :expt expt :instant instant)))

(defmacro defaults
  "Allows given variables to take on their default values"
  [& exprs]
  `(with-str-args defaults- ~exprs))

(defn change-day 
  "Sets active time n days ahead or behind."
  [n]
  (swap! active-time
         #(-> (parse-time %)
              (plus (days n))
              unparse-time)))

;;;;;;;;;;;;;;
;; Predicates
;;






;;;;;;;;;;;
;; Helpers
;;

(defn help
  "Informs you what's what."
  []
  (->> (for [[n v] (ns-interns 'familiar.core)]
         [(str "+ " n) "\n    " (:doc (meta v))])
       (remove #(nil? (nth % 2)))
       (interpose "\n")
       flatten
       println))

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

#_(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar"))

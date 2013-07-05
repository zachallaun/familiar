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

;; forward declarations required, just like in history class! cool!
(declare str->key active-expt display-vars active-expt active-expt-name)

(defrecord Variable [name validator default unit time-res tags])

;;;;;;;;;;;;;;;
;; Experiments
;;

(def db (atom (h2 {:db "data/default/default.db"})))

(defn open!
  "Changes active experiment to that with the specified name"
  [file]
  (reset! db (h2 {:db (str "data/" file  "/" file ".db")}))
  (defdb korma-db @db)
  (create-tables @db))

(open! "default")

;;;;;;;;;;;;;
;; Variables
;;

(defn tag-var
  "Adds tags to variable"
  [varname & tags]
  (assert (seq (get-field :name variable varname))
          (str "No variable by the name " varname "."))
  (apply create-if-missing tag tags)
  (for [name tags]
    (insert variable_tag
      (values {:tag_id      (get-field :id tag name)
               :variable_id (get-field :id variable varname)}))))

(defn add-var
  "Adds variable to experiment.
     Optional arguments:
     :expt - name of an experiment (defaults to loaded experiment)
     :time-res - time resolution (defaults to date, can be date-time)
     :unit - a string representing the unit of measure
     :tags - a sequence of strings with which to tag the variable"
  [name validator default
   & {:keys [expt time-res unit tags]
        :or {expt active-expt
             time-res "date"
             unit ""
             tags '()}}]
  (assert ((eval (read-string validator)) default)
          "Given default fails validator.")
  (insert variable
    (values {:name name
             :default default
             :unit unit
             :time-res time-res
             :validator validator}))
  (apply tag-var name tags))

(defn display-vars
  "Displays info for variables in active experiment" 
  []
  (->> (select variable (with tag))
       (map (comp
              #(select-keys % [:default :validator :unit :tag :name])
              (fn [t]
                (update-in t [:tag] #(map :name %)))))
       pprint))

(defn validate [varname value]
  (let [validator (-> (get-field :validator variable varname)
                      read-string
                      eval)]
    (validator value)))

;;;;;;;;
;; Data
;;

(defn add-datum
  "Adds a single instance of variable."
  [varname value & {:keys [expt instant]
                      :or {expt active-expt
                           instant @active-time}}]
  (assert (validate varname value)
          (str value " is invalid for " varname))
  (insert instance
    (values {:time instant
             :value value
             :variable_id (get-field :id variable varname)})))

(defn add-data 
  "Adds instances of variables with values.
     Example:
     (add-data [\"mice\" 6 \"cats\" 2 \"dogs\" 0])"
  [coll & {:keys [expt instant]
             :or {expt active-expt
                  instant @active-time}}]
  )

(defn missing-today
  "Displays all variables with no instance for the
     time pixel matching the active time/given time."
  [& {:keys [expt instant]
        :or {expt active-expt
             instant @active-time}}]
  )

(defn entered-today
  "Displays values for variables with an instance within
     the time pixel matching the active time/given time."
  [& {:keys [expt instant]
        :or {expt active-expt
             instant @active-time}}]
  )

(defn let-default 
  "Allows collection of variables to take on their default values"
  [variables & {:keys [expt instant]
                  :or {expt active-expt
                       instant @active-time}}]
  )

(defn change-day 
  "Sets active time n days ahead or behind."
  [n]
  )

;;;;;;;;;;;;;;
;; Predicates
;;






;;;;;;;;;;;
;; Helpers
;;

(defn help []
  (->> (for [[n v] (ns-publics 'familiar.core)]
         [(str "- " n) "\n    " (:doc (meta v))])
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

#_(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (println "Launching Familiar"))

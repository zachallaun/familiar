(ns familiar.dbconfig
  (:refer-clojure :exclude [alter drop bigint boolean 
                            char double float time])
  (:require [lobos
             [core :refer :all]
             [connectivity :refer :all]
             [schema :refer :all]]
            [korma.core :refer [defentity entity-fields
                                has-one has-many many-to-many
                                belongs-to select limit]]))

(def bit-bucket-writer
  (proxy [java.io.Writer] []
    (write [buf] nil)
    (close []    nil)
    (flush []    nil)))
 
(defmacro noprint
  "Evaluates the given expressions with all printing to *out* silenced."
  [& forms]
    `(binding [*out* bit-bucket-writer]
       ~@forms))

(declare variable instance tag experiment variable_tag variable_experiment)

(defentity variable
  (entity-fields :name :default :unit :time-res
                 :fn :variable_tag_id :deps)
  (many-to-many tag :variable_tag)
  (has-many instance))

(defentity instance
  (entity-fields :time :value :variable_id)
  (belongs-to variable {:fk :variable_id}))

(defentity tag
  (entity-fields :name :variable_tag_id)
  (many-to-many variable :variable_tag))

(defentity experiment
  (entity-fields :name :variable_experiment_id)
  (many-to-many variable :variable_experiment))

(defentity variable_tag
  (entity-fields :variable_id :tag_id))

(defentity variable_experiment
  (entity-fields :variable_id :experiment_id))

(defn surrogate-key [table]
  (integer table :id :auto-inc :primary-key))

(defn- refer-to [table ptable]
  (let [cname (-> (->> ptable name (apply str))
                  (str "_id")
                  keyword)]
    (integer table cname [:refer ptable :id :on-delete :set-null])))

(defn- refer-cascade [table ptable]
  (let [cname (-> (->> ptable name (apply str))
                  (str "_id")
                  keyword)]
    (integer table cname [:refer ptable :id :on-delete :cascade])))

(defmacro tbl [name & elements]
  `(-> (table ~name)
       ~@(reverse elements)
       (surrogate-key)))

(defmacro create-if-missing [tablename & tabledef]
  `(try
     (noprint (select ~tablename (limit 1)))
     (catch Exception e#
       (create (tbl ~(keyword tablename) ~@tabledef))
       (println (str "Creating table " '~tablename)))))

(defn create-tables [db]
  (try
    (close-global)
    (println "Closed open experiment")
    (catch Exception e))
  (open-global db)
  (let [len 100, llen 1000]
    (create-if-missing
      variable
      (varchar :time-res len)
      (varchar :fn llen)
      (varchar :default len)
      (varchar :unit len)
      (varchar :deps llen)
      (varchar :name len :unique))
    (create-if-missing
      instance
      (varchar :time len)
      (index :time_unique [:time :variable_id] :unique)
      (varchar :value len)
      (refer-to :variable))
    (create-if-missing
      tag
      (varchar :name len :unique))
    (create-if-missing
      variable_tag
      (refer-cascade :variable)
      (refer-cascade :tag))
    (create-if-missing
      experiment
      (varchar :name len :unique))
    (create-if-missing
      variable_experiment
      (refer-cascade :variable)
      (refer-cascade :experiment))))

(ns familiar.dbconfig
  (:refer-clojure :exclude [alter drop bigint boolean 
                            char double float time])
  (:require [lobos
             [core :refer :all]
             [connectivity :refer :all]
             [schema :refer :all]]
            [korma.core :refer [defentity entity-fields
                                has-one has-many many-to-many
                                belongs-to]]))

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

(declare variable tag instance variable_tag)

(defentity variable
  (entity-fields :name :default :unit :time-res
                 :validator :variable_tag_id)
  (many-to-many tag :variable_tag)
  (has-many instance))

(defentity instance
  (entity-fields :time :value :variable_id)
  (belongs-to variable {:fk :variable_id}))

(defentity tag
  (entity-fields :name :variable_tag_id)
  (many-to-many variable :variable_tag))

(defentity variable_tag
  (entity-fields :variable_id :tag_id))

(defn- surrogate-key [table]
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

(defn create-tables [db]
  (try
    (close-global)
    (println "Closed open experiment")
    (catch Throwable e (.getMessage e)))
  (open-global db)
  (try (noprint
    (create (tbl :variable
                 (varchar :time-res 100)
                 (varchar :validator 100)
                 (varchar :default 100)
                 (varchar :unit 100)
                 (varchar :name 100 :unique)))
    (create (tbl :instance
                 (varchar :time 100)
                 (index :time_unique [:time :variable_id] :unique)
                 (varchar :value 100)
                 (refer-to :variable)))
    (create (tbl :tag
                 (varchar :name 100)))
    (create (tbl :variable_tag
                 (refer-cascade :variable)
                 (refer-cascade :tag))))
    (catch Throwable e (println "Database found. Hooray."))))

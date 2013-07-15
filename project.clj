(defproject familiar "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/tools.namespace "0.2.3"]
                 [org.clojure/java.jdbc "0.3.0-alpha4"]
                 [korma "0.3.0-RC5"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 [lobos "1.0.0-beta1"]
                 [org.clojure/java.jdbc "0.3.0-alpha4"]
                 [com.h2database/h2 "1.3.170"]
                 [swiss-arrows "0.6.0"]
                 [clj-time "0.5.1"]
                 [jkkramer/loom "0.2.0"]]
  :main familiar.core)

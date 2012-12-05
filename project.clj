(defproject carrit "0.2-SNAPSHOT"
  :description "Minecraft Chunk Server"
  :java-source-paths ["java-src"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/algo.generic "0.1.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.clojure/tools.cli "0.2.1"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :main carrit.core)

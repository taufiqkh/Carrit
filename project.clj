(defproject carrit "0.1"
  :description "Minecraft Chunk Server"
  :java-source-path "java-src"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.generic "0.1.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [log4j "1.2.15" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :main carrit.chunk-world)

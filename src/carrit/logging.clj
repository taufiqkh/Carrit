(ns carrit.logging
  (:require [clojure.contrib.logging :as logging]))

; TODO: Change these to macros, so that the right namespace is logged
(defn info
  "Logs the specified string using a standard logger"
  [log-message & parameters]
  (logging/info (apply format log-message parameters)))

(defn debug
  "Logs the specified string using a standard logger with debug"
  [log-message & parameters]
  (logging/debug (apply format log-message parameters)))
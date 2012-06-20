(ns carrit.logging
  (:require [clojure.contrib.logging :as logging]))

; TODO: Change these to macros, so that the right namespace is logged
(defmacro info
  "Logs the specified string using a standard logger"
  [log-message & parameters]
  `(logging/info (format ~log-message ~@parameters)))

(defmacro debug
  "Logs the specified string using a standard logger with debug"
  [log-message & parameters]
  `(logging/debug (format ~log-message ~@parameters)))
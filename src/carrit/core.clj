(ns carrit.core
  (:use carrit.region-file)
  (:gen-class))

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def MINECRAFT_DIR "Whole New World")

(defn -main [& options]
  (if-let [save-dir (load-save-dir MINECRAFT_DIR)]
    (let [origin-descriptor (create-file-descriptor 0 0 0)]
      (read-region-file origin-descriptor ((save-dir :region-map) (origin-descriptor :filename)))))
  nil)
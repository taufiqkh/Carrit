(ns carrit.core
  (:use clojure.tools.cli
        clojure.tools.logging
        carrit.named-binary-tag
        carrit.region-file
        carrit.chunk-world)
  (:import java.io.File)
  (:gen-class))

; Minecraft save directory, hard-coded to a test directory until I get around
; to adding options
(def MINECRAFT_DIR "Whole New World")

(defn load-dir [& options]
  (if-let [save-dir (load-save-dir MINECRAFT_DIR)]
    (let [origin-descriptor (create-file-descriptor 0 0 0)]
      (read-region-file origin-descriptor ((save-dir :region-map) (origin-descriptor :filename)))))
  nil)

(defn -main [& args]
  (let [[options trailing-args usage] (cli args ["-n" "--nbt-tree" "Display NBT tree for the given file"])]
    (println (str options))
    (if (nil? (:nbt-tree options))
      (-loadChunkWorld nil MINECRAFT_DIR)
      (let [filename (:nbt-tree options)
            nbt-file (File. filename)
            nbt-bytes (slurp-binary-file! nbt-file)
            root-nbt (nbt-from-byte-array nbt-bytes 0)]
        (info "NBT Tree for " filename)
        (traverse root-nbt (fn [nbt] (println (str (:name nbt)))))))))
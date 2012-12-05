(ns carrit.test-region-file
  (:use clojure.test
        carrit.region-file
        [clojure.set :only (subset?)])
  (:import java.util.Arrays
           java.io.File))

; TODO: This could no doubt be done more succinctly
(deftest test-create-file-descriptor-from-coords
  "Given an x/y/z coordinate, ensure that the file descriptor is correct"
  (let [descriptor (create-file-descriptor 0 0 0)]
    (is (= (:filename descriptor) "r.0.0.mca"))
    (is (= (:xRegion descriptor) 0))
    (is (= (:zRegion descriptor) 0)))
  (let [descriptor (create-file-descriptor 0 128 0)]
    (is (= (:filename descriptor) "r.0.0.mca"))
    (is (= (:xRegion descriptor) 0))
    (is (= (:zRegion descriptor) 0)))
  (let [descriptor (create-file-descriptor -1 0 1)]
    (is (= (:filename descriptor) "r.-1.0.mca"))
    (is (= (:xRegion descriptor) -1))
    (is (= (:zRegion descriptor) 0)))
  (let [descriptor (create-file-descriptor 0 1 -1)]
    (is (= (:filename descriptor) "r.0.-1.mca"))
    (is (= (:xRegion descriptor) 0))
    (is (= (:zRegion descriptor) -1)))
  (let [descriptor (create-file-descriptor 128 1 -128)]
    (is (= (:filename descriptor) "r.4.-4.mca"))
    (is (= (:xRegion descriptor) 4))
    (is (= (:zRegion descriptor) -4))))

(deftest test-create-file-descriptor-from-filename
  "Given a filename, ensure that the file descriptor is correct"
  (let [filename "r.0.0.mca"
        descriptor (create-file-descriptor filename)]
    (is (= (:filename descriptor) filename))
    (is (= (:xRegion descriptor) 0))
    (is (= (:xRegion descriptor) 0)))
  (let [filename "r.-123.0.mca"
        descriptor (create-file-descriptor filename)]
    (is (= (:filename descriptor) filename))
    (is (= (:xRegion descriptor) -123))
    (is (= (:zRegion descriptor) 0)))
  (let [x -98912
        z -545
        filename (str "r." x "." z ".mca")
        descriptor (create-file-descriptor filename)]
    (is (= (:filename descriptor) filename))
    (is (= (:xRegion descriptor) x))
    (is (= (:zRegion descriptor) z))))

(deftest test-save-dir-files
  "Given a directory, ensure that the save file map returned is correct"
  (let [dir "Whole New World"
        files (save-dir-files dir)]
    (is (not (nil? files)))
    (let [save-files (:save-files files)
          region-files (:region-files files)]
      (is (not (nil? save-files)))
      (is (subset? #{"data" "level.dat" "region"} (set (keys save-files))))
      (is (not (nil? region-files)))
      (is (empty? (drop-while #(contains? #{"r.0.0.mca", "r.-1.-1.mca", "r.-1.0.mca", "r.0.-1.mca"} %) (keys region-files)))))))

; Disabled the following as the region file was too big
;(deftest test-read-region-file
;  (is (not (nil? (read-region-file (File. "test/resources/Test World/region/r.0.0.mca"))))))
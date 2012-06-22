(ns carrit.test-region-file
  (:use clojure.test
        carrit.region-file)
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
  (let [filename "r.-98912.-545.mca"
        descriptor (create-file-descriptor filename)]
    (is (= (:filename descriptor) filename))
    (is (= (:xRegion descriptor) -98912))
    (is (= (:zRegion descriptor) -545))))
  
(deftest test-read-region-file
  (is (not (nil? (read-region-file (File. "test/resources/Test World/region/r.0.0.mca"))))))
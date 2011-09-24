(ns carrit.test-core
  (:require [carrit.core :as core])
  (:use clojure.test)
  (:import java.util.Arrays))

(defn vec-as-byte-array [vec-to-convert]
  (byte-array (reduce #(conj %1 (byte %2)) [] vec-to-convert)))

(defmacro unsigned-byte [bval] (byte (if (> bval 127) (- bval 256) bval)))

(deftest test-expand-arrays
  (let [arrays [(vec-as-byte-array [1 2 3]) (vec-as-byte-array [4 5 6 7])]
        ^bytes expanded-array (core/expand-arrays arrays 5)]
    (is (= 5 (count expanded-array)))
    (is (Arrays/equals expanded-array ^bytes (vec-as-byte-array [1 2 3 4 5])))))

(deftest test-chunk-num-from-byte-array
  "Number from a byte array in the region file format"
  (is (= 262405 (core/chunk-num-from-byte-array (vec-as-byte-array [4 1 5]) 0 3)))
  (is (= 2560 (core/chunk-num-from-byte-array (vec-as-byte-array [0 0xa 0]) 0 3)))
  (is (= 2728 (core/chunk-num-from-byte-array (vec-as-byte-array [0 0xa (unsigned-byte 0xa8)]) 0 3))))
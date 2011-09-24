(ns carrit.test-core
  (:require [carrit.core :as core])
  (:use clojure.test)
  (:import java.util.Arrays))

(defn vec-as-byte-array [vec-to-convert]
  (byte-array (reduce #(conj %1 (byte %2)) [] vec-to-convert)))

(deftest test-expand-arrays
  (let [arrays [(vec-as-byte-array [1 2 3]) (vec-as-byte-array [4 5 6 7])]
        expanded-array (core/expand-arrays arrays 5)]
    (is (= 5 (count expanded-array)))
    (is (Arrays/equals expanded-array (vec-as-byte-array [1 2 3 4 5])))))
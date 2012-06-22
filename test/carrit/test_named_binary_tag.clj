(ns carrit.test-named-binary-tag
  (:require [clojure.java.io :as io])
  (:use clojure.test
        carrit.byte-convert
        carrit.named-binary-tag
        [carrit.region-file :only (slurp-binary-file!)])
  (:import java.io.File
           java.util.Arrays))

(set! *warn-on-reflection* true)

(defn reconstruct-utf-8 [^String string]
  (let [string-bytes (.getBytes string utf-8)]
  (read-utf-8-segment string-bytes 0 (alength string-bytes))))

(deftest test-str-from-byte-array
  "String from a byte array"
  (let [test-string "This is a string"]
    (is (= test-string (reconstruct-utf-8 test-string))))
  (let [test-string-bytes (.getBytes "κόσμε" utf-8)]
    (is (= (seq test-string-bytes)
           (seq (copy-from-byte-array test-string-bytes 0 (alength test-string-bytes))))))
  (let [test-string "κόσμε"]
    (is (= test-string (reconstruct-utf-8 test-string)))))

(deftest test-read-utf-8-name
  "Length-specified string from a byte array"
  (let [^String test-string "This is a string"
        test-string-array (.getBytes test-string utf-8)
        test-array (byte-array (+ (alength test-string-array) short-length))]
      (aset-byte test-array 0 0)
      (let [test-len (byte (alength test-string-array))]
        (aset-byte test-array 1 test-len)
        (is (= (num-from-byte-array test-array 0 short-length) test-len)))
      (doseq [idx (range (alength test-string-array))]
        (aset-byte test-array (+ idx 2) (aget test-string-array idx)))
      (is (not (nil? (extract-utf-8-name test-array 0))))))

(deftest test-read-int-array-extract
  "Read an IntArray extract from a byte array"
  (let [length 2
        ^bytes chunk-bytes (byte-array (map byte [0 0 0 length
                                                  0 0 0 2
                                                  0 0 0 1])) ; 4 initial bytes for array length
        extract (extract-from-byte-array type-int-array ^bytes chunk-bytes 0)]
    (is (= (+ int-length (* int-length length)) (:length extract)))
    (let [nbt (:data extract)
          ^ints nbt-array (:data nbt)]
      (is (= type-int-array (:type nbt)))
      (is (= length (alength nbt-array)))
      (is (Arrays/equals (int-array length [2 1]) nbt-array)))))

(deftest test-read-compound-extract-nbt
  "Read a compound extract from a byte array"
  (let [^bytes chunk-bytes (byte-array (map unchecked-byte [type-compound
                                                            0 4
                                                            116 101 115 116 ; "test"
                                                            type-string
                                                            0 1
                                                            97
                                                            0 9
                                                            0x73 0x68 0x6F 0x72 0x74 0x54 0x65 0x73 0x74 ; "shortTest"
                                                            type-end]))
        extract (extract-nbt-from-byte-array chunk-bytes 0)
        compound-nbt (:data extract)
        children (:data compound-nbt)]
    (is (= (:name compound-nbt) "test"))
    (is (contains? children "a"))
    (is (= (children "a")))))

(deftest integration-test-nbt
  "Full test with an NBT file"
  (let [test-file (File. "test/resources/test.nbt") chunk-byte-array (slurp-binary-file! test-file)]
    (is (not (nil? (nbt-from-byte-array chunk-byte-array 0))))))

(deftest integration-test-big-nbt
  "Full test with a big NBT file"
  (let [test-file (File. "test/resources/bigtest.nbt") chunk-byte-array (slurp-binary-file! test-file)]
    (is (not (nil? (nbt-from-byte-array chunk-byte-array 0))))))
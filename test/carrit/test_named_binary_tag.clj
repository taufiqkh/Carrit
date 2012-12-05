(ns carrit.test-named-binary-tag
  (:require [clojure.java.io :as io])
  (:use clojure.test
        carrit.byte-convert
        carrit.named-binary-tag
        [carrit.region-file :only (slurp-binary-file!)]
        clojure.pprint)
  (:import java.io.File
           java.util.Arrays))

(set! *warn-on-reflection* true)

(defn verify-nbt
  ([nbt type data name child-type]
    (is (not (nil? nbt)))
    (is (not-empty nbt))
    (is (every? (partial contains? nbt) [:type :data]))
    (is (= name (:name nbt)))
    (is (= child-type (:child-type nbt)))
    (if (contains? #{type-byte type-short type-int type-long type-float type-double} type)
      (is (= data (:data nbt))))
    )
  ([nbt type data name]
    (verify-nbt nbt type data name nil))
  ([nbt type data]
    (verify-nbt nbt type data nil nil)))

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

(deftest test-read-double-extract
  "Read a double from a byte array"
  (let [nbt-bytes (byte-array (map unchecked-byte [0x43 0x30 0x89 0x15 0x53 0xD5 0xE8 0x22]))
        extract (extract-from-byte-array type-double nbt-bytes 0)]
    (verify-nbt (:data extract) type-double 4.654324321216546E15)))

(deftest test-read-list-extract
  "Read a list from a byte array"
  (let [length 2
        data [type-compound
              0 0 0 length
              type-byte
              0 4 ; length of byte name
              116 101 115 116 ; "test"
              1 ; byte data
              type-string
              0 1 ; length of name
              97 ; "a"
              0 9 ; length of string data
              0x73 0x68 0x6F 0x72 0x74 0x54 0x65 0x73 0x74 ; "shortTest"
              type-end
              type-int
              0 5 ; length of int name
              116 101 115 116 50 ; "test2"
              0 0 0 2 ; int data
              type-long
              0 5 ; length of long name
              116 101 115 116 51 ; "test3"
              1 2 3 4 5 6 7 8 ; 72623859790382856L
              type-end]
        ^bytes chunk-bytes (byte-array (map unchecked-byte data))
        extract (extract-from-byte-array type-list chunk-bytes 0)
        nbt (:data extract)
        nbt-data (:data nbt)]
    (is (not (nil? extract)))
    (is (not (nil? nbt-data)))
    (let [first-compound-nbt (first nbt-data)
          first-compound-data (:data first-compound-nbt)
          second-compound-nbt (second nbt-data)
          second-compound-data (:data second-compound-nbt)]
      (is (nil? (:name first-compound-nbt)))
      (is (nil? (:name second-compound-nbt)))
      (is (= 2 (count (keys first-compound-data))))
      (is (= 2 (count (keys second-compound-data))))
      (let [test-byte-nbt (first-compound-data "test")
            test-string-nbt (first-compound-data "a")
            test-int-nbt (second-compound-data "test2")
            test-long-nbt (second-compound-data "test3")]
        (verify-nbt test-byte-nbt type-byte 1 "test")
        (verify-nbt test-string-nbt type-string "shortTest" "a")
        (verify-nbt test-int-nbt type-int (int 2) "test2")
        (verify-nbt test-long-nbt type-long (long 72623859790382856) "test3")
    ))))

(deftest test-read-int-array-extract
  "Read an IntArray extract from a byte array"
  (let [length 2
        data [0 0 0 length
              0 0 0 2
              0 0 0 1]
        ^bytes chunk-bytes (byte-array (map byte data)) ; 4 initial bytes for array length
        extract (extract-from-byte-array type-int-array ^bytes chunk-bytes 0)]
    (is (= (+ int-length (* int-length length)) (:length extract)))
    (let [nbt (:data extract)
          ^ints nbt-array (:data nbt)]
      (is (= type-int-array (:type nbt)))
      (is (= length (alength nbt-array)))
      (is (Arrays/equals (int-array length [2 1]) nbt-array)))))

(deftest test-read-compound-extract-nbt
  "Read a compound extract from a byte array"
  (let [data [type-compound
              0 4 ; length of name
              116 101 115 116 ; "test"
              type-string
              0 1 ; length of name
              97 ; "a"
              0 9 ; length of string data
              0x73 0x68 0x6F 0x72 0x74 0x54 0x65 0x73 0x74 ; "shortTest"
              type-end]
        ^bytes chunk-bytes (byte-array (map unchecked-byte data))
        extract (extract-nbt-from-byte-array chunk-bytes 0)
        compound-nbt (:data extract)
        children (:data compound-nbt)]
    (is (= (:name compound-nbt) "test"))
    (is (contains? children "a"))
    (is (= (:data (children "a")) "shortTest"))))

(deftest integration-test-nbt
  "Full test with an NBT file"
  (let [test-file (File. "test/resources/test.nbt")
        chunk-byte-array (slurp-binary-file! test-file)
        root-nbt (nbt-from-byte-array chunk-byte-array 0)]
    (is (not (nil? root-nbt)))))

(deftest integration-test-big-nbt
  "Full test with a big NBT file"
  (let [test-file (File. "test/resources/bigtest.nbt")
        chunk-byte-array (slurp-binary-file! test-file)
        root-nbt (nbt-from-byte-array chunk-byte-array 0)
        root-nbt-data (:data root-nbt)]
    (is (not (nil? root-nbt)))
    (is (= "Level" (:name root-nbt)))
    (is (= 11 (count (keys (:data root-nbt)))))
    (let [nested-compound ((:data root-nbt) "nested compound test")]
      (is (not (nil? nested-compound))))
    (let [nested-list-compound ((:data root-nbt) "listTest (compound)")]
      (is (not (nil? nested-list-compound)))
      (is (= 2 (count (:data nested-list-compound))))
      (let [first-nbt (first (:data nested-list-compound))
            second-nbt (second (:data nested-list-compound))]
        (let [first-data (:data first-nbt)
              first-created-on (first-data "created-on")
              first-name (first-data "name")]
          (is (not (nil? first-created-on)))
          (is (not (nil? first-name))))))))
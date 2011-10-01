(ns carrit.named-binary-tag
  "Named Binary Tag format functions"
  (:use carrit.byte-convert))

(set! *warn-on-reflection* true)

(def ^{:doc "Tag end, denoting the end of a list."} END 0)
(def ^{:doc "Single, signed byte."} BYTE 1)
(def ^{:doc "Signed short, 16 bits."} SHORT 2)
(def ^{:doc "Signed int, 32 bits."} INT 3)
(def ^{:doc "Signed long, 64 bits."} LONG 4)
(def ^{:doc "Float, 32 bits IEEE 754-2008."} FLOAT 5)
(def ^{:doc "Double, 64 bits IEEE 754-2008."} DOUBLE 6)
(def ^{:doc "Byte array of unspecified format."} BYTE_ARRAY 7)
(def ^{:doc "String."} STRING 8)
(def ^{:doc "Sequential list of a specified type."} LIST 9)
(def ^{:doc "Compound tag, which is a sequential list of uniquely named tags."} COMPOUND 10)

(defrecord NamedBinaryTag [type name payload])

(defn as-nbt [^bytes chunk-bytes]
  (let [nbt-type (aget chunk-bytes 0)]
    (if (= nbt-type COMPOUND)
      (NamedBinaryTag. COMPOUND (num-from-byte-array chunk-bytes 1 4) nil)
      nil)))
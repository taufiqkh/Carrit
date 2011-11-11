(ns carrit.named-binary-tag
  "Named Binary Tag format functions"
  (:use carrit.byte-convert)
  (:import java.util.Arrays))

(set! *warn-on-reflection* true)

(def ^{:doc "Tag end, denoting the end of a list."} END 0)
(def ^{:doc "Single, signed byte."} BYTE 1)
(def ^{:doc "Signed short, 16 bits."} SHORT 2)
(def ^{:doc "Signed int, 32 bits."} INT 3)
(def ^{:doc "Signed long, 64 bits."} LONG 4)
(def ^{:doc "Float, 32 bits IEEE 754-2008."} FLOAT 5)
(def ^{:doc "Double, 64 bits IEEE 754-2008."} DOUBLE 6)
(def ^{:doc "Byte array of unspecified format."} BYTE_ARRAY 7)
(def ^{:doc "String."} *string* 8)
(def ^{:doc "Sequential list of a specified type."} LIST 9)
(def ^{:doc "Compound tag, which is a sequential list of uniquely named tags."} *compound* 10)
(def ^{:doc "Length of a short."} *length-short*)

(def ^{:doc "UTF-8 encoding"} *utf-8* "UTF-8")

(defrecord NamedBinaryTag [type name payload])

(defmulti nbt-from-byte-array type)

; TODO: Better way to do this?
(defn copy-from-byte-array [^bytes chunk-bytes ^Integer idx length]
  "Copies UTF-8 characters from a byte array and returns them as a string"
  (Arrays/copyOfRange chunk-bytes idx #^Integer (+ idx length)))

(defn byte-array-utf-8 [^bytes chunk-bytes idx code-points]
  (let [^bytes dest (copy-from-byte-array chunk-bytes idx code-points)]
    (String. dest #^String *utf-8*)))

(defmethod nbt-from-byte-array *string* [^bytes chunk-bytes idx]
  (let [length (num-from-byte-array idx *length-short*)]
    nil)) ;

(defn as-nbt [^bytes chunk-bytes]
  (let [nbt-type (aget chunk-bytes 0)]
    (if (= nbt-type *compound*)
      (NamedBinaryTag. *compound* (num-from-byte-array chunk-bytes 1 *length-short*) nil)
      nil)))

(defn compound-nbt-from-byte-array [^bytes chunk-bytes idx]
  (let [nbt-type (aget chunk-bytes idx)]
    (if (= nbt-type *compound*)
      (NamedBinaryTag. *compound* (nbt-from-byte-array *string* chunk-bytes (inc idx)) nil)
      nil)))

(defn string-from-byte-array [^bytes chunk-bytes idx]
  (let [length (num-from-byte-array idx *length-short*)]
    nil)) ;
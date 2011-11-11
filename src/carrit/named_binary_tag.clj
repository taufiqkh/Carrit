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
(def ^{:doc "String."} *string* 8)
(def ^{:doc "Sequential list of a specified type."} LIST 9)
(def ^{:doc "Compound tag, which is a sequential list of uniquely named tags."} *compound* 10)
(def ^{:doc "Length of a short."} *length-short*)

(def ^{:doc "UTF-8 encoding"} *utf-8* "UTF-8")

(defrecord NamedBinaryTag [type name payload])

(defmulti nbt-from-byte-array type)

(defn conj-next-utf-8-char [^bytes chunk-bytes idx byte-vec]
  (let [next-byte (aget chunk-bytes idx) new-vec (conj byte-vec next-byte)]
    (if (= (bit-and (unsigned-byte-to-num next-byte) 0x80) 0x80)
      (recur chunk-bytes (inc idx) new-vec)
      new-vec)))

; TODO: Are there existing functions for this?
(defn byte-array-to-vector [^bytes chunk-bytes idx chars-remaining byte-vec]
  "Reads chars-remaining UTF-8 characters from a byte array and returns them as a vector"
  (if (zero? chars-remaining)
    byte-vec
    (recur chunk-bytes (inc idx) (dec chars-remaining) (conj-next-utf-8-char chunk-bytes idx byte-vec))))

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
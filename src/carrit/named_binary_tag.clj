(ns carrit.named-binary-tag
  "Named Binary Tag format functions, as conforming to the NBT format specified at http://mc.kev009.com/NBT"
  (:use carrit.byte-convert
        carrit.logging)
  (:import java.util.Arrays))

(set! *warn-on-reflection* true)

; TODO: Add mapping to clojure entities

(def ^{:doc "Tag end, denoting the end of a compound tag list."} *end* 0)
(def ^{:doc "Single, signed byte."} *byte* 1)
(def ^{:doc "Signed short, 16 bits."} *short* 2)
(def ^{:doc "Signed int, 32 bits."} *int* 3)
(def ^{:doc "Signed long, 64 bits."} *long* 4)
(def ^{:doc "Float, 32 bits IEEE 754-2008."} *float* 5)
(def ^{:doc "Double, 64 bits IEEE 754-2008."} *double* 6)
(def ^{:doc "Byte array of unspecified format."} *byte-array* 7)
(def ^{:doc "String."} *string* 8)
(def ^{:doc "Sequential list of a specified type."} *list* 9)
(def ^{:doc "Compound tag, which is a sequential list of uniquely named tags."} *compound* 10)
(def ^{:doc "Length-prefixed array of signed, 4 byte integers"} *int-array* 11)

(def *type-lengths* (hash-map *end* 1
                   *byte* 1
                   *short* *short-length*
                   *int* *int-length*
                   *long* *long-length*
                   *float* *float-length*
                   *double* *double-length*))

(def ^{:doc "UTF-8 encoding" :tag String} *utf-8* "UTF-8")

(defrecord NamedBinaryTag [type name extract])

(defrecord ^{:doc "Extract from a byte array, containing copied data and the read length, in bytes"} Extract [data length])

(defn copy-from-byte-array [^bytes chunk-bytes ^Integer idx length]
  "Copies bytes from an array and returns them as a new array."
  (Arrays/copyOfRange chunk-bytes idx #^Integer (+ idx length)))

(defn read-utf-8-segment [^bytes chunk-bytes idx byte-length]
  "Reads the specified number of bytes at the given index in the array and
returns it as a UTF-8 string."
  (let [^bytes buffer (copy-from-byte-array chunk-bytes idx byte-length)]
    (String. buffer #^String *utf-8*)))

(defn read-utf-8-name [^bytes chunk-bytes idx]
  "Read a tag name from the byte array at the specified index, returning the name and length, in bytes."
  (let [length (num-from-byte-array chunk-bytes idx *short-length*)]
    (Extract. (read-utf-8-segment chunk-bytes (+ idx *short-length*) length) length)))

; Given a byte array, read the extract for the specified type and return that
; extract and the length, in bytes, of the section that was read.
(defmulti extract-from-byte-array (fn [tag-type chunk-bytes idx] tag-type))

(defn read-nbt-from-byte-array
  ([^bytes chunk-bytes idx]
    "Reads an NBT from the given chunk-bytes byte array, starting at the specified index."
    (let [nbt-type (aget chunk-bytes idx)]
      ; (logging/debug (apply format "nbt type is %d, index %d" [nbt-type idx]))
      (if (= nbt-type *end*)
        (Extract. (NamedBinaryTag. nbt-type nil nil) 1)
        (let [name-idx (inc idx)
              nbt-name-data (read-utf-8-name chunk-bytes name-idx)
              ; Extract index starts after type, name length and name
              extract-data (extract-from-byte-array nbt-type chunk-bytes (+ idx 1 *short-length* (:length nbt-name-data)))]
          ; (logging/debug nbt-name-data)
          (Extract. (NamedBinaryTag. nbt-type (:data nbt-name-data) (:data extract-data))
                    ; tag id length + "name length" length + name length + extract length
                    (+ 1 *short-length* (:length nbt-name-data) (:length extract-data))))))))

(defn nbt-from-byte-array [^bytes chunk-bytes idx]
  (:data (read-nbt-from-byte-array chunk-bytes idx)))

(defmethod extract-from-byte-array *byte* [tag-type ^bytes chunk-bytes idx]
  (Extract. (aget chunk-bytes idx) 1))

(defmethod extract-from-byte-array *byte-array* [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx *int-length*)]
    (Extract. (copy-from-byte-array chunk-bytes (+ idx *int-length*) length)
              (+ *int-length* length))))

(defmethod extract-from-byte-array *string* [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx *short-length*)]
    (Extract. (read-utf-8-segment chunk-bytes (+ idx *short-length*) length)
              (+ *short-length* length))))

(defmethod extract-from-byte-array *list* [tag-type ^bytes chunk-bytes idx]
  (let [list-tag-type (aget chunk-bytes idx)
        list-length (num-from-byte-array chunk-bytes (inc idx) *int-length*)]
    (loop [num-left list-length next-idx (+ idx 1 *int-length*) acc []]
      (if (zero? num-left)
        (Extract. acc (- next-idx idx))
        (let [extract-data (extract-from-byte-array list-tag-type chunk-bytes next-idx)]
          (recur (dec num-left) (+ next-idx (:length extract-data)) (conj acc (:data extract-data))))))))

(defmethod extract-from-byte-array *compound* [tag-type ^bytes chunk-bytes idx]
  (loop [nbt-meta (read-nbt-from-byte-array chunk-bytes idx) acc [] length-acc 0]
    (let [nbt (:data nbt-meta) nbt-length (:length nbt-meta)] 
      (if (= (:type nbt) *end*)
        (Extract. (conj acc nbt) (+ length-acc nbt-length))
        (recur (read-nbt-from-byte-array chunk-bytes (+ idx length-acc nbt-length))
               (conj acc nbt)
               (+ length-acc nbt-length))))))

(defmethod extract-from-byte-array *int-array* [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx *int-length*)
        data (int-array length)]
    (doseq [^Integer int-idx (range 0 length)]
             (aset data int-idx ^Integer (num-from-byte-array chunk-bytes (+ idx *int-length* (* int-idx *int-length*)) *int-length*)))
    (Extract. data (+ *int-length* (* *int-length* length)))))

(defmethod extract-from-byte-array :default [tag-type ^bytes chunk-bytes idx]
  (let [type-length (get *type-lengths* (int tag-type))]
    (Extract. (num-from-byte-array chunk-bytes idx type-length) type-length)))

(defn retrieve-tag [nbt-compound nbt-name]
  "Retrieves the tag with the specified name from the NBT Compound tag"
  (loop [remaining-data (:data nbt-compound) remaining-length (:length nbt-compound)]
    (if (zero? remaining-length)
      nil
      (if-let [nbt-tag (= (:name (peek remaining-data)) nbt-name)]
        nbt-tag
        (recur (pop remaining-data) (dec remaining-length))))))

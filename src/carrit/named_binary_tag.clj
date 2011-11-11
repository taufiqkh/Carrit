(ns carrit.named-binary-tag
  "Named Binary Tag format functions"
  (:use carrit.byte-convert)
  (:import java.util.Arrays))

(set! *warn-on-reflection* true)

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

(def ^{:doc "UTF-8 encoding"} *utf-8* "UTF-8")

(defrecord NamedBinaryTag [type name payload])

; TODO: Better way to do this?
(defn copy-from-byte-array [^bytes chunk-bytes ^Integer idx length]
  "Copies bytes from an array and returns them as a new array."
  (Arrays/copyOfRange chunk-bytes idx #^Integer (+ idx length)))

(defn byte-array-utf-8 [^bytes chunk-bytes idx byte-length]
  "Reads the specified number of bytes at the given index in the array and
returns it as a UTF-8 string."
  (let [^bytes dest (copy-from-byte-array chunk-bytes idx byte-length)]
    (String. dest #^String *utf-8*)))

(defn read-utf-8 [^bytes chunk-bytes idx]
  "Read a tag name from the byte array at the specified index, returning the name and length, in bytes."
  (let [length (num-from-byte-array chunk-bytes idx Short/SIZE)]
    {:data (byte-array-utf-8 chunk-bytes idx length) :length length}))

; TODO: Any way to do a partial of read-fn with read-length before passing it in?
(defn read-nbt-from-byte-array
  [nbt-type chunk-bytes idx read-fn]
  (let [string-data (read-utf-8 chunk-bytes idx)]
    (NamedBinaryTag. nbt-type (string-data :data) (read-fn chunk-bytes (+ idx (string-data :length))))))

; Given a byte array, read the payload for the specified type and return that
; payload and the length, in bytes, of the section that was read.
(defmulti payload-from-byte-array identity)

(defn read-nbt-from-byte-array [^bytes chunk-bytes idx]
  (let [nbt-type (aget chunk-bytes idx)]
    (if (= nbt-type *end*)
      {:nbt (NamedBinaryTag. nbt-type nil nil) :length 1}
      (let [nbt-name-data (read-utf-8 chunk-bytes (inc idx))
            ; payload index starts after type, name length and name
            payload-data (payload-from-byte-array nbt-type chunk-bytes (+ idx 2 (nbt-name-data :length)))]
        {:nbt (NamedBinaryTag. nbt-type (nbt-name-data :data) (payload-data :data))
         :length (+ 1 (nbt-name-data :length) (payload-data :length))}))))

(defn nbt-from-byte-array [^bytes chunk-bytes idx]
  ((read-nbt-from-byte-array chunk-bytes idx) :data))

(defmethod payload-from-byte-array *byte* [^bytes chunk-bytes idx]
  {:data (aget chunk-bytes idx) :length 1})

(defmethod payload-from-byte-array *short* [^bytes chunk-bytes idx]
  {:data (num-from-byte-array chunk-bytes idx Short/SIZE) :length Short/SIZE})

(defmethod payload-from-byte-array *int* [^bytes chunk-bytes idx]
  {:data (num-from-byte-array chunk-bytes idx Integer/SIZE) :length Integer/SIZE})

(defmethod payload-from-byte-array *long* [^bytes chunk-bytes idx]
  {:data (num-from-byte-array chunk-bytes idx Long/SIZE) :length Long/SIZE})

(defmethod payload-from-byte-array *float* [^bytes chunk-bytes idx]
  {:data (num-from-byte-array chunk-bytes idx Float/SIZE) :length Float/SIZE})

(defmethod payload-from-byte-array *double* [^bytes chunk-bytes idx]
  {:data (num-from-byte-array chunk-bytes idx Double/SIZE) :length Double/SIZE})

(defmethod payload-from-byte-array *byte-array* [^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx Integer/SIZE)]
    {:data (copy-from-byte-array chunk-bytes (+ idx Integer/SIZE) length)
     :length (+ Integer/SIZE length)}))

(defmethod payload-from-byte-array *string* [^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx Short/SIZE)]
    {:data (byte-array-utf-8 chunk-bytes (+ idx length Short/SIZE) length)
     :length (+ Short/SIZE length)}))

(defmethod payload-from-byte-array *list* [^bytes chunk-bytes idx]
  (let [tag-id (aget chunk-bytes idx)
        list-length (num-from-byte-array chunk-bytes (inc idx) Integer/SIZE)]
    (loop [num-left list-length next-idx (+ idx 1 Integer/SIZE) acc []]
      (if (zero? num-left)
        acc
        (let [payload-data (payload-from-byte-array tag-id chunk-bytes next-idx)]
          (recur (dec num-left) (conj acc (payload-data :data)) (+ next-idx payload-data :length)))))))

(defmethod payload-from-byte-array *compound* [^bytes chunk-bytes idx]
  (loop [nbt-meta (read-nbt-from-byte-array chunk-bytes idx) acc [] length-acc 0]
    (let [nbt (nbt-meta :data) nbt-length (nbt-meta :length)] 
      (if (= (nbt :type) *end*)
        {:data (conj acc nbt) :length (+ length-acc nbt-length)}
        (recur (nbt-from-byte-array chunk-bytes (+ idx length-acc nbt-length))
               (conj acc nbt)
               (+ length-acc nbt-length))))))

(defn as-nbt [^bytes chunk-bytes]
  (let [nbt-type (aget chunk-bytes 0)]
    (if (= nbt-type *compound*)
      (NamedBinaryTag. *compound* (num-from-byte-array chunk-bytes 1 Short/SIZE) nil)
      nil)))
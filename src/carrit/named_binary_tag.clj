(ns carrit.named-binary-tag
  "Named Binary Tag format functions, as conforming to the NBT format specified at http://mc.kev009.com/NBT"
  (:use carrit.byte-convert))

(set! *warn-on-reflection* true)

; TODO: Add mapping to clojure entities

(def ^{:doc "Tag end, denoting the end of a compound tag list."} type-end 0)
(def ^{:doc "Single, signed byte."} type-byte 1)
(def ^{:doc "Signed short, 16 bits."} type-short 2)
(def ^{:doc "Signed int, 32 bits."} type-int 3)
(def ^{:doc "Signed long, 64 bits."} type-long 4)
(def ^{:doc "Float, 32 bits IEEE 754-2008."} type-float 5)
(def ^{:doc "Double, 64 bits IEEE 754-2008."} type-double 6)
(def ^{:doc "Byte array of unspecified format."} type-byte-array 7)
(def ^{:doc "String."} type-string 8)
(def ^{:doc "Sequential list of a specified type."} type-list 9)
(def ^{:doc "Compound tag, which is a sequential list of uniquely named tags."} type-compound 10)
(def ^{:doc "Length-prefixed array of signed, 4 byte integers"} type-int-array 11)

(def type-lengths (hash-map type-end 1
                   type-byte 1
                   type-short short-length
                   type-int int-length
                   type-long long-length
                   type-float float-length
                   type-double double-length))

(def ^{:doc "UTF-8 encoding" :tag String} utf-8 "UTF-8")

(defrecord NamedBinaryTag [type data name])

(defrecord ^{:doc "Extract from a byte array, containing copied data and the read length, in bytes"}
            Extract [data length])

(defn read-utf-8-segment [^bytes chunk-bytes idx byte-length]
  "Reads the specified number of bytes at the given index in the array and
returns it as a UTF-8 string."
  (let [^bytes buffer (copy-from-byte-array chunk-bytes idx byte-length)]
    (String. buffer #^String utf-8)))

(defn extract-utf-8-name [^bytes chunk-bytes idx]
  "Read a tag name from the byte array at the specified index, returning the name and length, in bytes. The tag name is
expected to be prefixed by the length of the name."
  (let [length (num-from-byte-array chunk-bytes idx short-length)]
    (Extract. (read-utf-8-segment chunk-bytes (+ idx short-length) length) length)))

; Given a byte array, read the extract for the specified type and return that
; extract and the length, in bytes, of the section that was read.
(defmulti extract-from-byte-array (fn [tag-type chunk-bytes idx] tag-type))

(defn extract-nbt-from-byte-array
  ([^bytes chunk-bytes idx]
    "Reads an NBT from the given chunk-bytes byte array, starting at the specified index."
    (let [nbt-type (aget chunk-bytes idx)]
      ; (logging/debug (apply format "nbt type is %d, index %d" [nbt-type idx]))
      (if (= (int nbt-type) type-end)
        (Extract. (NamedBinaryTag. nbt-type nil nil) 1)
        (let [name-idx (inc idx)
              nbt-name (extract-utf-8-name chunk-bytes name-idx)
              ; Extract index starts after type, name length and name
              extract (extract-from-byte-array nbt-type chunk-bytes (+ idx 1 short-length (:length nbt-name)))]
          ; (logging/debug nbt-name-data)
          (Extract. (NamedBinaryTag. nbt-type (:data extract) (:data nbt-name))
                    ; tag id length + "name length" length + name length + extract length
                    (+ 1 short-length (:length nbt-name) (:length extract))))))))

(defn nbt-from-byte-array [^bytes chunk-bytes idx]
  (:data (extract-nbt-from-byte-array chunk-bytes idx)))

(defmethod extract-from-byte-array type-byte [tag-type ^bytes chunk-bytes idx]
  (Extract. (NamedBinaryTag. type-byte (aget chunk-bytes idx) nil) 1))

(defmethod extract-from-byte-array type-byte-array [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx int-length)
        start-idx (+ idx int-length)]
    (Extract. (reduce (fn [byte-seq byte-idx]
                        (conj byte-seq (aget chunk-bytes byte-idx)))
                      []
                      (range start-idx (+ start-idx length)))
              (+ int-length length))))

(defmethod extract-from-byte-array type-string [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx short-length)]
    (Extract. (read-utf-8-segment chunk-bytes (+ idx short-length) length)
              (+ short-length length))))

(defmethod extract-from-byte-array type-list [tag-type ^bytes chunk-bytes idx]
  (let [list-tag-type (aget chunk-bytes idx)
        list-length (num-from-byte-array chunk-bytes (inc idx) int-length)]
    (loop [num-left list-length next-idx (+ idx 1 int-length) acc []]
      (if (zero? num-left)
        (Extract. acc (- next-idx idx))
        (let [extract-data (extract-from-byte-array list-tag-type chunk-bytes next-idx)]
          (recur (dec num-left) (+ next-idx (:length extract-data)) (conj acc (:data extract-data))))))))

(defmethod extract-from-byte-array type-compound [tag-type ^bytes chunk-bytes idx]
  (loop [extract (extract-nbt-from-byte-array chunk-bytes idx) acc [] length-acc 0]
    (let [nbt (:data extract) nbt-length (long (:length extract))] 
      (if (= (:type nbt) type-end)
        (Extract. (conj acc nbt) (+ length-acc nbt-length))
        (recur (extract-nbt-from-byte-array chunk-bytes (+ idx length-acc nbt-length))
               (conj acc nbt)
               (+ length-acc nbt-length))))))

(defmethod extract-from-byte-array type-int-array [tag-type ^bytes chunk-bytes idx]
  (let [length (num-from-byte-array chunk-bytes idx int-length)
        data (int-array length)]
    (doseq [^Integer int-idx (range 0 length)]
             (aset data int-idx ^Integer (num-from-byte-array chunk-bytes (+ idx int-length (* int-idx int-length)) int-length)))
    (Extract. data (+ int-length (* int-length length)))))

(defmethod extract-from-byte-array :default [tag-type ^bytes chunk-bytes idx]
  (let [type-length (get type-lengths (int tag-type))]
    (Extract. (num-from-byte-array chunk-bytes idx type-length) type-length)))

(defn retrieve-tag [nbt-compound nbt-name]
  "Retrieves the tag with the specified name from the NBT Compound tag"
  (loop [remaining-data (:data nbt-compound) remaining-length (:length nbt-compound)]
    (if (zero? remaining-length)
      nil
      (if-let [nbt-tag (= (:name (peek remaining-data)) nbt-name)]
        nbt-tag
        (recur (pop remaining-data) (dec remaining-length))))))

(ns carrit.byte-convert
  (:import java.util.Arrays))

(set! *warn-on-reflection* true)

(def ^{:doc "Length of a short, in bytes"} short-length (/ Short/SIZE 8))
(def ^{:doc "Length of an int, in bytes"} int-length (/ Integer/SIZE 8))
(def ^{:doc "Length of a long, in bytes"} long-length (/ Long/SIZE 8))
(def ^{:doc "Length of a float, in bytes"} float-length (/ Float/SIZE 8))
(def ^{:doc "Length of a double, in bytes"} double-length (/ Double/SIZE 8))

(defmacro unsigned-byte-to-num [bval]
  `(bit-and 0xff (int ~bval)))

(defn num-from-byte-array
  ([^bytes chunk-byte-array idx]
    "Returns a number from a big-endian chunk byte array using the byte at the specified index"
    (unsigned-byte-to-num (aget chunk-byte-array idx)))
  ([^bytes chunk-byte-array start-index length]
    "Returns a number from a big-endian chunk byte array using entries up to the
specified length. The length of the array must be at least the specified
length."
    {:pre [(>= (alength chunk-byte-array) (dec (+ start-index length)))]}
    (let [end-index (dec (+ start-index length))] 
      ; Bit shift left by byte size for each digit until we reach the end
      (reduce + (for [i (range start-index (inc end-index))]
                  (bit-shift-left (unsigned-byte-to-num (aget chunk-byte-array i)) (* (- end-index i) 8)))))))

(defn copy-from-byte-array [^bytes bytes ^Integer idx length]
  "Copies bytes from an array and returns them as a new array."
  (Arrays/copyOfRange bytes idx #^Integer (+ idx length)))

(ns carrit.byte-convert
  (:import java.util.Arrays))

(set! *warn-on-reflection* true)

(def ^:const ^{:doc "Length of a short, in bytes"} short-length (/ Short/SIZE 8))
(def ^:const ^{:doc "Length of an int, in bytes"} int-length (/ Integer/SIZE 8))
(def ^:const ^{:doc "Length of a long, in bytes"} long-length (/ Long/SIZE 8))
(def ^:const ^{:doc "Length of a float, in bytes"} float-length (/ Float/SIZE 8))
(def ^:const ^{:doc "Length of a double, in bytes"} double-length (/ Double/SIZE 8))

(defmacro unsigned-byte-to-num
  "Returns an unsigned number from an unsigned byte."
  [bval]
  `(bit-and 0xff (long ~bval)))

(defn num-from-byte-array
  "Returns a signed number from a big-endian byte array using entries up
to the specified length. The length of the array must be at least the specified
length."
  [^bytes from-byte-array start-index length]
  {:pre [(>= (alength from-byte-array) (+ start-index length))]}
  (let [end-index (dec (+ start-index length))] 
    ; Bit shift left by byte size for each digit until we reach the end
    (reduce bit-or
            (bit-shift-left (long (aget from-byte-array start-index)) (* (dec length) 8))
            (for [i (range (inc start-index) (inc end-index))]
              (bit-shift-left (unsigned-byte-to-num (aget from-byte-array i)) (* (- end-index i) 8))))))

(defn float-from-byte-array
  "Returns a float from a big-endian byte array conforming to IEEE 754
floating-point \"single format\" bit layout. The length of the array must
be at least as long as a float."
  [from-byte-array start-index]
  (Float/intBitsToFloat (num-from-byte-array from-byte-array start-index float-length)))

(defn double-from-byte-array
  "Returns a float from a big-endian byte array conforming to IEEE 754
floating-point \"double format\" bit layout. The length of the array must
be at least as long as a double."
  [from-byte-array start-index]
  (Double/longBitsToDouble (long (num-from-byte-array from-byte-array start-index double-length))))

(defn copy-from-byte-array
  "Copies bytes from an array and returns them as a new array."
  [^bytes bytes ^Integer idx length]
  (Arrays/copyOfRange bytes idx #^Integer (+ idx length)))

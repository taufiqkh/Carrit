(ns carrit.byte-convert)

(set! *warn-on-reflection* true)

; TODO: If possible, change to macro
(defn unsigned-byte-to-num [bval]
  "Converts a byte to a number using unsigned arithmetic"
  (if (< bval 0) (+ bval 256) bval))

(defmacro unsigned-byte [bval] (byte (if (> bval 127) (- bval 256) bval)))

(defn num-from-byte-array [^bytes chunk-byte-array start-index length]
  "Returns a number from a big-endian chunk byte array using entries up to the
specified length. The length of the array must be at least the specified
length."
  {:pre [(>= (alength chunk-byte-array) (dec (+ start-index length)))]}
  (let [end-index (dec (+ start-index length))] 
    ; Bit shift left by byte size for each digit until we reach the end
    (reduce + (for [i (range start-index (inc end-index))]
                (bit-shift-left (unsigned-byte-to-num (aget chunk-byte-array i)) (* (- end-index i) 8))))))
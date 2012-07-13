(ns carrit.test-byte-convert
  (:use clojure.test
        carrit.byte-convert)
  (:import java.util.Arrays))

(defn- byte-arr
  "Creates a byte array with the unchecked bytes from the given vector"
  [vec]
  (byte-array (map unchecked-byte vec)))

(defn- byte-arr-string ; debug function
  [byte-arr]
  (map #(String/format "0x%02X" (object-array [(aget byte-arr %)])) (range (alength byte-arr))))

(defn- verify-array
  [expected-vec actual-array]
  (is (Arrays/equals (byte-arr expected-vec) actual-array)))

(deftest test-unsigned-byte-to-num
  (let [large-unsigned (unchecked-byte 0xE7)
        small-unsigned (unchecked-byte 0x03)]
    (is (= 231 (unsigned-byte-to-num large-unsigned)))
    (is (= 3 (unsigned-byte-to-num small-unsigned)))))

(deftest test-byte-array-to-short
  (let [test-array (byte-arr [0xF1 0x01 0xF7 0x00 0x00 0xFF 0xFF 0x00])
        verify-short (fn [val idx] (is (= val (num-from-byte-array test-array idx short-length))))]
    (verify-short 503 1)
    (verify-short -3839 0)
    (verify-short 0 3)
    (verify-short 255 4)
    (verify-short -1 5)))

(deftest test-short-as-byte-array
  (verify-array [0x01 0xF7] (short-as-byte-array 503))
  (verify-array [0xF1 0x01] (short-as-byte-array -3893)))

(deftest test-int-from-byte-array
  (let [test-array (byte-arr[0xF1 0x01 0xE7 0x03 0x04])]
    (is (= 31916804 (num-from-byte-array test-array 1 int-length)))
    (is (= -251533565 (num-from-byte-array test-array 0 int-length)))))

(deftest test-int-as-byte-array
  (verify-array [0xF1 0x01 0xE7 0x03] (int-as-byte-array -251533565))
  (verify-array [0x01 0xE7 0x03 0x04] (int-as-byte-array 31916804)))

(deftest test-long-from-byte-array
  (let [test-array (byte-arr [0xF1 0x00 0xE7 0x03 0x04 0x00 0xFF 0xFF 0xFF])]
    (is (= 65024035351691263 (num-from-byte-array test-array 1 long-length)))
    (is (= -1080609910430826497 (num-from-byte-array test-array 0 long-length)))))

(deftest test-float-limits-from-byte-array
  (let [positive-infinity (byte-arr [0xF1 0x7F 0x80 0x00 0x00 0xFF])
        negative-infinity (byte-arr [0x07 0xFF 0x80 0x00 0x00 0xF6])
        min-value (byte-arr [0x00 0x00 0x00 0x01])
        max-value (byte-arr [0x7F 0x7F 0xFF 0xFF])
        min-normal (byte-arr [0x00 0x80 0x00 0x00])]
    (is (= Float/POSITIVE_INFINITY (byte-array-to-float positive-infinity 1)))
    (is (= Float/NEGATIVE_INFINITY (byte-array-to-float negative-infinity 1)))
    (is (= Float/MIN_VALUE (byte-array-to-float min-value 0)))
    (is (= Float/MAX_VALUE (byte-array-to-float max-value 0)))
    (is (= Float/MIN_NORMAL (byte-array-to-float min-normal 0)))))

(deftest test-float-nan-from-byte-array
  (let [nan (byte-arr [0x7F 0xC0 0x00 0x00 0xF3])
        nan2 (byte-arr [0xFF 0x80 0xF0 0xC1 0x0F])
        nan3 (byte-arr [0xFF 0xFF 0xFF 0xFF])]
    (is (Float/isNaN (byte-array-to-float nan 0)))
    (is (Float/isNaN (byte-array-to-float nan2 0)))
    (is (Float/isNaN (byte-array-to-float nan3 0)))))

(deftest test-float-values-from-byte-array
  (let [val (byte-arr [0x3E 0x20 0x00 0x00])]
    (is (= 0.15625 (byte-array-to-float val 0)))))

(deftest test-float-from-extended-byte-array
  (let [val-pre-ff (byte-arr [0xFF 0x3E 0x20 0x00 0x00])
        val-pre-00 (byte-arr [0x00 0x3E 0x20 0x00 0x00])
        val-post-ff (byte-arr [0x3E 0x20 0x00 0x00 0xFF])
        val-post-00 (byte-arr [0x3E 0x20 0x00 0x00 0x00])
        val-pre-post (byte-arr [0xF0 0x3E 0x20 0x00 0x00 0x00 0xFF])
        verify-float (fn [arr idx] (is (= 0.15625 (byte-array-to-float arr idx))))]
    (verify-float val-pre-ff 1)
    (verify-float val-pre-00 1)
    (verify-float val-post-ff 0)
    (verify-float val-post-00 0)
    (verify-float val-pre-post 1)))

(deftest test-double-limits-from-byte-array
  (let [positive-infinity (byte-arr [0x7F 0xF0 0x00 0x00 0x00 0x00 0x00 0x00])
        negative-infinity (byte-arr [0xFF 0xF0 0x00 0x00 0x00 0x00 0x00 0x00])
        min-value (byte-arr [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01])
        max-value (byte-arr [0x7F 0xEF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])
        min-normal (byte-arr [0x00 0x10 0x00 0x00 0x00 0x00 0x00 0x00])]
    (is (= Double/POSITIVE_INFINITY (byte-array-to-double positive-infinity 0)))
    (is (= Double/NEGATIVE_INFINITY (byte-array-to-double negative-infinity 0)))
    (is (= Double/MIN_VALUE (byte-array-to-double min-value 0)))
    (is (= Double/MAX_VALUE (byte-array-to-double max-value 0)))
    (is (= Double/MIN_NORMAL (byte-array-to-double min-normal 0)))))

(deftest test-double-nan-from-byte-array
  (let [nan (byte-arr [0x7F 0xF8 0x00 0x00 0x00 0x00 0x00 0x00])
        nan2 (byte-arr [0x7F 0xFF 0xA0 0x00 0xDE 0xAD 0xBE 0xEF])
        nan3 (byte-arr [0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])]
    (is (Double/isNaN (byte-array-to-double nan 0)))
    (is (Double/isNaN (byte-array-to-double nan2 0)))
    (is (Double/isNaN (byte-array-to-double nan3 0)))))

(deftest test-double-value-from-byte-array
  (let [val (byte-arr [0x43 0x30 0x89 0x15 0x53 0xD5 0xE8 0x22])]
    (is (= 4.654324321216546E15 (byte-array-to-double val 0)))))
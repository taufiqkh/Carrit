(ns carrit.test-byte-convert
  (:use clojure.test
        carrit.byte-convert))

(deftest test-unsigned-byte-to-num
  (let [large-unsigned (unchecked-byte 0xE7)
        small-unsigned (unchecked-byte 0x03)]
    (is (= 231 (unsigned-byte-to-num large-unsigned)))
    (is (= 3 (unsigned-byte-to-num small-unsigned)))))

(deftest test-short-from-byte-array
  (let [test-array (byte-array (map unchecked-byte [0xF1 0x01 0xF7 0x00 0x00 0xFF 0xFF 0x00]))
        verify-short (fn [val idx] (is (= val (num-from-byte-array test-array idx short-length))))]
    (verify-short 503 1)
    (verify-short -3839 0)
    (verify-short 0 3)
    (verify-short 255 4)
    (verify-short -1 5)))

(deftest test-int-from-byte-array
  (let [test-array (byte-array (map unchecked-byte [0xF1 0x01 0xE7 0x03 0x04]))]
    (is (= 31916804 (num-from-byte-array test-array 1 int-length)))
    (is (= -251533565 (num-from-byte-array test-array 0 int-length)))))

(deftest test-long-from-byte-array
  (let [test-array (byte-array (map unchecked-byte [0xF1 0x00 0xE7 0x03 0x04 0x00 0xFF 0xFF 0xFF]))]
    (is (= 65024035351691263 (num-from-byte-array test-array 1 long-length)))
    (is (= -1080609910430826497 (num-from-byte-array test-array 0 long-length)))))

(deftest test-float-from-byte-array
  (let [positive-infinity (byte-array (map unchecked-byte [0xF1 0x7F 0x80 0x00 0x00 0xFF]))
        negative-infinity (byte-array (map unchecked-byte [0x07 0xFF 0x80 0x00 0x00 0xF6]))
        nan (byte-array (map unchecked-byte [0x7F 0xC0 0x00 0x00 0xF3]))
        nan2 (byte-array (map unchecked-byte [0xFF 0x80 0xF0 0xC1 0x0F]))
        nan3 (byte-array (map unchecked-byte [0xFF 0xFF 0xFF 0xFF]))
        min-value (byte-array (map unchecked-byte [0x00 0x00 0x00 0x01]))
        max-value (byte-array (map unchecked-byte [0x7F 0x7F 0xFF 0xFF]))
        min-normal (byte-array (map unchecked-byte [0x00 0x80 0x00 0x00]))
        val (byte-array (map unchecked-byte [0x3E 0x20 0x00 0x00]))]
    (is (= Float/POSITIVE_INFINITY (float-from-byte-array positive-infinity 1)))
    (is (= Float/NEGATIVE_INFINITY (float-from-byte-array negative-infinity 1)))
    (is (Float/isNaN (float-from-byte-array nan 0)))
    (is (Float/isNaN (float-from-byte-array nan2 0)))
    (is (Float/isNaN (float-from-byte-array nan3 0)))
    (is (= Float/MIN_VALUE (float-from-byte-array min-value 0)))
    (is (= Float/MAX_VALUE (float-from-byte-array max-value 0)))
    (is (= Float/MIN_NORMAL (float-from-byte-array min-normal 0)))
    (is (= 0.15625 (float-from-byte-array val 0)))))
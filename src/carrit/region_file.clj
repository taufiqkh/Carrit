(ns carrit.region-file
  (:require [clojure.java.io :as io]
            [clojure.algo.generic.math-functions :as math]
            [clojure.set :as set])
  (:import java.io.File
           java.util.Arrays
           java.util.zip.Inflater)
  (:use carrit.named-binary-tag
        carrit.byte-convert
        [clojure.tools.logging :only (info debug)]))
(set! *warn-on-reflection* true)
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def expected-save-entries #{"region" "level.dat"})

(defrecord FileDescriptor [filename xRegion zRegion])

(defrecord Region [filename x z chunks])

(defn slurp-binary-file! [^File file]
  (io! (with-open [reader (io/input-stream file)]
         (let [buffer (byte-array (.length file))]
           (.read reader buffer)
           buffer))))

(defn verify-save-dir [^File directory]
  "Verifies that a given directory contains the files/directories expected in a
save game directory and returns a sequence of those files."
  (io!
    (info "Verifying directory " (.getPath directory))
    (if (.isDirectory directory)
      (let [files (file-seq directory) filenames (set (map (fn [^File file] (.getName file)) files))]
        (if (set/subset? expected-save-entries filenames)
          files
          nil))
      nil)))

(defn map-region-dir [files]
  "Given a save game directory finds the region directory and extracts the
files for that directory, mapped by file name."
  (if (empty? files)
    (info "Files exhausted before region directory found.")
    (let [^File file (first files)]
      (io!
        (if (= "region" (.getName file))
          (if (.isDirectory file)
            (let [region-file-seq (file-seq file)
                  region-file-names (map #(.getName ^File %) region-file-seq)]
              (zipmap region-file-names region-file-seq))
            nil)
          (recur (next files)))))))

(def ^{:doc "Bitshift for x chunk to determine file name"} chunk-x-shift 5)
(def
  ^{:doc "Number of chunks per region along the x axis. Offset multiplier for x location for chunk location headers"}
   chunk-x-multiplier (int (math/pow 2 chunk-x-shift)))
(def ^{:doc "Bitshift for z chunk to determine file name"} chunk-z-shift 5)
(def
  ^{:doc "Number of chunks per region along the z axis. Offset multiplier for z location for chunk location headers"}
   chunk-z-multiplier (int (math/pow 2 chunk-z-shift)))
(def ^{:doc "Size of chunk location offset, in bytes"} chunk-location-offset-size 3)
(def ^{:doc "Size of chunk location sector, in bytes"} chunk-location-sector-size 1)
(def ^{:doc "Size of chunk location data, in bytes" }
      chunk-location-size
  (+ chunk-location-offset-size chunk-location-sector-size))
(def ^{:doc "Size of a chunk sector in bytes"} chunk-sector-size 4096)
(def ^{:doc "Size of chunk timestamp data, in bytes"} chunk-timestamp-size 4)
(def ^{:doc "Size of the length header for chunk data, in bytes"} chunk-data-lengh-header-size 4)
(def ^{:doc "Size of the compression type header, in bytes"} chunk-compression-type-size 1)
(def ^{:doc "Number of chunks in a file"} chunks-per-region (* chunk-x-multiplier chunk-z-multiplier))

(defn calc-chunk-header-offset [header-size x z]
  "Given the x, y, z coordinates, calculates the offset, in bytes, of the
chunk header in a region file"
  (* header-size (+ (mod x chunk-x-multiplier) (* (mod z chunk-z-multiplier) chunk-x-multiplier))))

; TODO: probably not needed, call underlying function directly
(defn chunk-timestamp [^bytes timestamp-byte-array]
  "Returns a chunk location from the specified timestamp byte array. The array
must be at least the size of a timestamp."
  (num-from-byte-array timestamp-byte-array 0 chunk-timestamp-size))

(defn chunk-length-byte-array [^bytes chunk-length-byte-array]
  "Returns the chunk length from the specified chunk header array. The array
must be at least the size of a chunk length header."
  (num-from-byte-array chunk-length-byte-array 0 chunk-data-lengh-header-size))

(defn chunk-range [region shift]
  "Derives a chunk coordinate range from a region"
  (range (bit-shift-left region shift) (bit-shift-left (inc region) shift)))

(defn region-loc-keys [descriptor]
  "Creates a set of location keys of [x, z] for all x, z in the region described by the specified descriptor"
  (let [x-range (chunk-range (:xRegion descriptor) chunk-x-shift)
        z-range (chunk-range (:zRegion descriptor) chunk-z-shift)]
    ; TODO: Explore less imperative way of writing this, something like:
    ;loc-key (reduce conj (map (fn [x inner-z] (map (fn [z] (vector x z)) inner-z)) x-range (repeat z-range)))]
    ; Need to rewrite for clarity:
    (loop [x-rem x-range
           loc-key-acc []]
      (if (empty? x-rem)
        loc-key-acc
        (recur (rest x-rem)
          (loop [x (first x-rem)
                 z-rem z-range
                 inner-loc-key-acc loc-key-acc]
            (if (empty? z-rem)
              inner-loc-key-acc
              (recur x (next z-rem) (conj inner-loc-key-acc [(int (first x-rem)) (int (first z-rem))])))))))))

(defn expand-arrays [arrays ^Integer length]
  "For a given sequence of arrays, returns a single array of the specified
length, containing the contents of the arrays up to that length."
  {:pre [(> length 0) (<= length (reduce + 0 (map #(alength ^bytes %) arrays)))]}
  (let [^bytes first-array (first arrays)
        ^bytes destination (Arrays/copyOf first-array length)]
    (loop [arrays-rem (rest arrays) offset (alength first-array) length-remaining (- length offset)]
      (if (empty? arrays-rem)
        destination
        (let [^bytes next-array (first arrays-rem) next-length (min (alength next-array) length-remaining)]
          (System/arraycopy next-array 0 destination offset next-length)
          (recur (rest arrays-rem) (+ offset (alength next-array)) (- length-remaining next-length)))))))

(defn inflate-chunk-data [chunk-byte-array offset compressed-length alloc-length]
  "Reads chunk data from the specified array, using the given offset and length"
  (debug "Inflating chunk data offset " offset
         " compressed-length " compressed-length
         " alloc-length " alloc-length)
  (if (<= compressed-length 0)
    {:length 0 :data nil}
    (let [inflater (Inflater.)]
      (.setInput inflater chunk-byte-array offset compressed-length)
      (loop [arrays [] next-byte-array (byte-array chunk-sector-size)]
        (.inflate inflater next-byte-array 0 chunk-sector-size)
        (let [bytes-read (.getBytesRead inflater)]
          (if (or (.finished inflater) (= bytes-read compressed-length))
            (let [bytes-written (.getBytesWritten inflater)]
              {:length bytes-written :data (expand-arrays arrays bytes-written)})
            (recur (conj arrays next-byte-array) (byte-array chunk-sector-size))))))))

(defn read-chunk-data [chunk-byte-array offset alloc-length]
  {:pre [(>= offset 0) (> alloc-length 0)]}
  "Reads the chunk header and data at a specified offset. Contains side effects."
  (let [compressed-length (num-from-byte-array chunk-byte-array offset chunk-data-lengh-header-size)
        compression-type (num-from-byte-array chunk-byte-array offset chunk-compression-type-size)
        data-offset (+ offset chunk-data-lengh-header-size chunk-compression-type-size)
        inflated-data (inflate-chunk-data chunk-byte-array data-offset (dec compressed-length) alloc-length)]
    {:compressed-length compressed-length,
     :compression-type compression-type,
     :data (nbt-from-byte-array (:data inflated-data) 0),
     :length (:length inflated-data)}))

(defn read-chunk [chunk-byte-array location-offset timestamp-offset]
  "Reads chunk data from the specified offsets, returning a chunk map with the
following keys:
  data-offset
  sectors
  timestamp
and the following additional keys if the chunk map has been generated
  compressed-length
  compression-type
  data
  length"
  (let [data-location (* (num-from-byte-array chunk-byte-array location-offset chunk-location-offset-size)
                         chunk-sector-size)
        sectors (num-from-byte-array chunk-byte-array
                                           (+ location-offset chunk-location-offset-size)
                                           chunk-location-sector-size)
        chunk-map {:data-offset data-location
                   :sectors sectors
                   :timestamp (num-from-byte-array chunk-byte-array timestamp-offset chunk-timestamp-size)}]
    (if (zero? data-location)
      chunk-map
      (let []
        (debug "Offset: " data-location " Sectors: " sectors " Timestamp: " (chunk-map :timestamp))
        (merge chunk-map (read-chunk-data chunk-byte-array data-location (* sectors chunk-sector-size)))))))

(defn region-coordinates [x y z]
  "Given x, y and z coordinates, returns the a map of the base coordinates of the region containing that position."
  (let [shift-map {:x chunk-x-shift, :z chunk-z-shift}
        val-map {:x x, :z z}
        [xRegion zRegion] (map #(bit-shift-right (val-map %) (shift-map %)) [:x :z])]
    {:x xRegion :z zRegion :y y}))

(defn create-file-descriptor
  ([filename]
    (if-let [[x z] (map read-string (next (re-find #"r\.(-?[0-9]+)\.(-?[0-9]+)\.mca" filename)))]
      (FileDescriptor. filename x z)))
  ([x y z]
    "Generates a region file name for the specified coordinates."
    ; Region determined by right bit shifting x and z
    (let [[x z] (map (region-coordinates x y z) [:x :z])]
      (FileDescriptor. (format "r.%d.%d.mca" x z) x z))))

(defn read-region-file
  ([file-descriptor file]
    "Reads a region file, returning a region map of [x z] to chunk-maps. Contains side-effects"
    (let [chunk-byte-array (slurp-binary-file! file)
          loc-keys (region-loc-keys file-descriptor)
          timestamp-header-offset (* chunks-per-region chunk-location-size)]
      (loop [region (Region. (:filename file-descriptor) (:xRegion file-descriptor) (:zRegion file-descriptor) {})
           location-read-from 0
           timestamp-read-from (* chunks-per-region chunk-location-size)
           loc-keys-rem loc-keys]
        (if (empty? loc-keys-rem)
          region
          (let [[x z] (peek loc-keys-rem)]
            (recur (assoc region
                          :chunks
                          (assoc (:chunks region)
                                 [x z]
                                 (read-chunk chunk-byte-array location-read-from timestamp-read-from)))
                   (long (calc-chunk-header-offset chunk-location-size x z))
                   (+ timestamp-header-offset (calc-chunk-header-offset chunk-timestamp-size x z))
                   (pop loc-keys-rem)))))))
  ([^File file]
    (read-region-file (create-file-descriptor (.getName file)) file)))

(defn save-dir-files [^String dirname]
  (if-let [save-dir-files (verify-save-dir (File. dirname))]
    (if-let [region-dir-files (map-region-dir save-dir-files)]
      {:save-dir save-dir-files
       :region-files region-dir-files}
      (info "Couldn't map region directory files for " dirname "."))
    nil))
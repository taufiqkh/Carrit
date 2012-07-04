(ns carrit.region-file
  (:require [clojure.java.io :as io]
            [clojure.algo.generic.math-functions :as math]
            [clojure.set :as set])
  (:import java.io.File
           java.util.Arrays
           java.util.zip.Inflater)
  (:use carrit.named-binary-tag
        carrit.byte-convert
        [clojure.tools.logging :only (info debug warn)]))
(set! *warn-on-reflection* true)
; Not idiomatic clojure; that will have to wait until I learn how to do things properly.
  
(def expected-save-entries #{"region" "level.dat"})

(defrecord FileDescriptor [filename xRegion zRegion])

(defrecord Region [filename x z chunks])

(defrecord ChunkMap [data-offset sectors timestamp compressed-length compression-type nbt length])

(defn- make-chunk-map
  ([data-offset sectors timestamp compressed-length compression-type nbt length]
    (ChunkMap. data-offset sectors timestamp compressed-length compression-type nbt length))
  ([data-offset sectors timestamp]
    (ChunkMap. data-offset sectors timestamp nil nil nil nil)))

(defn slurp-binary-file!
  "Slurps the specified binary file into a byte array which is then returned."
  [^File file]
  (io! (with-open [reader (io/input-stream file)]
         (let [buffer (byte-array (.length file))]
           (.read reader buffer)
           buffer))))

(defn- map-filenames-to-files [files]
  (zipmap (map #(.getName ^File %) files) files))

(defn verify-save-dir
  "Verifies that a given directory contains the files/directories expected in a
save game directory and returns a sequence of those files."
  [^File directory]
  (io!
    (info "Verifying directory " (.getPath directory))
    (if (.isDirectory directory)
      (let [files (map-filenames-to-files (seq (.listFiles directory)))
            filenames (set (keys files))
            not-present (set/difference expected-save-entries filenames)]
        (if (empty? not-present)
          files
          (warn "Couldn't find all expected files in save directory" (.getPath directory) ":" not-present)))
      nil)))

(defn map-region-dir
  "Given a save game directory finds the region directory and extracts the
files for that directory, mapped by file name."
  [files]
  (if (empty? files)
    (info "Files exhausted before region directory found.")
    (let [^File file (first files)]
      (io!
        (if (= "region" (.getName file))
          (if (.isDirectory file)
            (let [region-file-seq (seq (.listFiles file))
                  region-file-names (map #(.getName ^File %) region-file-seq)]
              (zipmap region-file-names region-file-seq))
            nil)
          (recur (next files)))))))

(def ^{:doc "Bitshift for x chunk to determine file name"} chunk-x-shift 5)
(def
  ^{:doc "Number of chunks per region along the x axis. Offset multiplier for x location for chunk location headers"}
   chunk-x-multiplier (long (math/pow 2 chunk-x-shift)))
(def ^{:doc "Bitshift for z chunk to determine file name"} chunk-z-shift 5)
(def
  ^{:doc "Number of chunks per region along the z axis. Offset multiplier for z location for chunk location headers"}
   chunk-z-multiplier (long (math/pow 2 chunk-z-shift)))
(def ^{:doc "Size of chunk location offset, in bytes"} chunk-location-offset-size 3)
(def ^{:doc "Size of chunk location sector, in bytes"} chunk-location-sector-size 1)
(def ^{:doc "Size of chunk location data, in bytes" }
      chunk-location-size
  (+ chunk-location-offset-size chunk-location-sector-size))
(def ^{:doc "Size of a chunk sector in bytes"} chunk-sector-size 4096)
(def ^{:doc "Size of chunk timestamp data, in bytes"} chunk-timestamp-size 4)
(def ^{:doc "Size of the length header for chunk data, in bytes"} chunk-data-length-header-size 4)
(def ^{:doc "Size of the compression type header, in bytes"} chunk-compression-type-size 1)
(def ^{:doc "Number of chunks in a file"} chunks-per-region (* chunk-x-multiplier chunk-z-multiplier))

(defn calc-chunk-header-offset
  "Given the x, y, z coordinates, calculates the offset, in bytes, of the
chunk header in a region file"
  [header-size x z]
  (* header-size (+ (mod x chunk-x-multiplier) (* (mod z chunk-z-multiplier) chunk-x-multiplier))))

(defn chunk-length-byte-array
  "Returns the chunk length from the specified chunk header array. The array
must be at least the size of a chunk length header."
  [^bytes chunk-length-byte-array]
  (num-from-byte-array chunk-length-byte-array 0 chunk-data-length-header-size))

(defn chunk-range
  "Derives a chunk coordinate range from a region"
  [region shift]
  (range (bit-shift-left region shift) (bit-shift-left (inc region) shift)))

(defn region-loc-keys
  "Creates a set of location keys of [x, z] for all x, z in the region described by the specified descriptor"
  [descriptor]
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
              (recur x (next z-rem) (conj inner-loc-key-acc [(long (first x-rem)) (long (first z-rem))])))))))))

(defn expand-arrays
  "For a given sequence of arrays, returns a single array of the specified
length, containing the contents of the arrays up to that length."
  [arrays ^Integer length]
  {:pre [(> length 0) (<= length (reduce + 0 (map #(alength ^bytes %) arrays)))]}
  (let [^bytes first-array (first arrays)
        ^bytes destination (Arrays/copyOf first-array length)]
    (loop [arrays-rem (rest arrays) offset (alength first-array) length-remaining (- length offset)]
      (if (empty? arrays-rem)
        destination
        (let [^bytes next-array (first arrays-rem) next-length (min (alength next-array) length-remaining)]
          (System/arraycopy next-array 0 destination offset next-length)
          (recur (rest arrays-rem) (+ offset (alength next-array)) (- length-remaining next-length)))))))

(defn inflate-chunk-data
  "Reads chunk data from the specified array, using the given offset and length"
  [chunk-byte-array offset compressed-length alloc-length]
  (debug "Inflating chunk data offset " offset
         " compressed-length " compressed-length
         " alloc-length " alloc-length)
  (if (<= compressed-length 0)
    {:length 0 :data nil}
    (let [inflater (Inflater.)]
      (.setInput inflater chunk-byte-array offset compressed-length)
      (loop [arrays [] next-byte-array (byte-array chunk-sector-size)]
        (.inflate inflater next-byte-array 0 chunk-sector-size)
        (let [bytes-read (.getBytesRead inflater)
              next-arrays (conj arrays next-byte-array)]
          (if (or (.finished inflater) (= bytes-read compressed-length))
            (let [bytes-written (.getBytesWritten inflater)]
              {:length bytes-written :data (expand-arrays next-arrays bytes-written)})
            (recur next-arrays (byte-array chunk-sector-size))))))))

(defn read-chunk-data
  "Reads the chunk header and data at a specified offset. Contains side effects."
  [chunk-byte-array offset alloc-length]
  {:pre [(>= offset 0) (> alloc-length 0)]}
  (let [compressed-length (num-from-byte-array chunk-byte-array offset chunk-data-length-header-size)
        compression-type (num-from-byte-array chunk-byte-array offset chunk-compression-type-size)
        data-offset (+ offset chunk-data-length-header-size chunk-compression-type-size)
        inflated-data (inflate-chunk-data chunk-byte-array data-offset (dec compressed-length) alloc-length)]
    {:compressed-length. compressed-length
     :compression-type compression-type
     :nbt (nbt-from-byte-array (:data inflated-data) 0)
     :length (:length inflated-data)}))

(defn read-chunk
  "Reads chunk data from the specified offsets, returning a ChunkMap record. Note that :data-offset will be zero and
:data will be nil if the chunk map has not been generated."
  [chunk-byte-array location-offset timestamp-offset]
  (let [data-location (* (num-from-byte-array chunk-byte-array location-offset chunk-location-offset-size)
                         chunk-sector-size)
        sectors (num-from-byte-array chunk-byte-array
                                     (+ location-offset chunk-location-offset-size)
                                     chunk-location-sector-size)
        chunk-map (make-chunk-map data-location
                                  sectors
                                  (num-from-byte-array chunk-byte-array timestamp-offset chunk-timestamp-size))]
    (if (zero? data-location)
      chunk-map
      (let []
        (debug "Offset: " data-location " Sectors: " sectors " Timestamp: " (chunk-map :timestamp))
        (merge chunk-map (read-chunk-data chunk-byte-array data-location (* sectors chunk-sector-size)))))))

(defn region-coordinates
  "Given x, y and z coordinates, returns the a map of the base coordinates of the region containing that position."
  [x y z]
  (let [shift-map {:x chunk-x-shift, :z chunk-z-shift}
        val-map {:x x, :z z}
        [xRegion zRegion] (map #(bit-shift-right (val-map %) (shift-map %)) [:x :z])]
    {:x xRegion :z zRegion :y y}))

(defn create-file-descriptor
  "Given a filename, derives the x, y and z coordinates and generates and
returns the file descriptor.
Given x, y and z coordinates, derives the region file name and generates
;and returns the appropriately populated file descriptor."
  ([filename]
    (if-let [[x z] (map read-string (next (re-find #"r\.(-?[0-9]+)\.(-?[0-9]+)\.mca" filename)))]
      (FileDescriptor. filename x z)))
  ([x y z]
    ; Region determined by right bit shifting x and z
    (let [[x z] (map (region-coordinates x y z) [:x :z])]
      (FileDescriptor. (format "r.%d.%d.mca" x z) x z))))

(defn read-region-file
  "Reads a region file, returning a region map of [x z] to chunk-maps for all chunks that are present. Contains
IO and side-effects"
  ([file-descriptor file]
    (let [chunk-byte-array (slurp-binary-file! file)
          loc-keys (region-loc-keys file-descriptor)
          timestamp-header-offset (* chunks-per-region chunk-location-size)]
      (debug "Reading region for descriptor" file-descriptor)
      (debug "Chunk byte array" chunk-byte-array)
      (loop [region (Region. (:filename file-descriptor) (:xRegion file-descriptor) (:zRegion file-descriptor) {})
           location-read-from 0
           timestamp-read-from (* chunks-per-region chunk-location-size)
           loc-keys-rem loc-keys]
        (if (empty? loc-keys-rem)
          region
          (let [[x z] (peek loc-keys-rem)
                chunk (read-chunk chunk-byte-array location-read-from timestamp-read-from)]
            (recur (assoc region
                          :chunks
                          (assoc (:chunks region) [x z] chunk))
                   (long (calc-chunk-header-offset chunk-location-size x z))
                   (+ timestamp-header-offset (calc-chunk-header-offset chunk-timestamp-size x z))
                   (pop loc-keys-rem)))))))
  ([^File file]
    (read-region-file (create-file-descriptor (.getName file)) file)))

(defn save-dir-files [^String dirname]
  (if-let [save-files (verify-save-dir (File. dirname))]
    (if-let [region-files (map-region-dir (vals save-files))]
      {:save-files save-files
       :region-files region-files}
      (info "Couldn't map region directory files for " dirname "."))
    nil))
(ns comfort.io
  (:require [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
  (:import (java.io File)
           (java.nio.file FileSystems)))

; Files

(defn ext
  "Find files of a particular extension (omit period) in dirname.
   Case insensitive. No guarantee which file wins if only differ by case!
   Map basenames (modified by namer, default `keyword`) to `File`s.
   e.g. (ext \"csv\" \"path/to/dir\")"
  ([extension dirname] (ext extension keyword dirname))
  ([extension namer dirname]
   (into (sorted-map)
         (for [file (.listFiles (io/file dirname))
               :let [filename (s/lower-case (.getName file))
                     extension (s/lower-case (str \. extension))]
               :when (s/ends-with? filename extension)
               :let [name (namer (subs filename 0 (s/last-index-of filename extension)))]]
           [name file]))))

(defn glob
  "Find files matching (possibly subdirectory-matching) glob expression.
   Root path is prepended to glob expression.
   e.g. (glob \"path/to/dir\" \"**/*.csv\")"
  [root pattern]
  {:pre [(some-> root io/file .isDirectory)
         (string? pattern)]}
  (let [root-path (->> root io/file .getPath)
        full-pattern (str root-path File/separator pattern)
        matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" full-pattern))
        matches (fn [file] (.matches matcher (.toPath file)))]
    (->> root-path io/file file-seq (filter matches))))

; Input

(defn csv-row-seq
  "Lazy-load whole table. `namer` transforms column names.
   Should work on file or reader. Closes reader at end."
  ([file] (csv-row-seq file keyword))
  ([file namer]
   (let [csvr (io/reader file)
         [header & data] (csv/read-csv csvr)
         fieldnames (map namer header)
         extract (fn continue [data]
                   (lazy-seq (if-let [row (first data)]
                               (cons (apply sorted-map (interleave fieldnames (map s/trim row)))
                                     (continue (next data)))
                               (do (.close csvr) nil))))]
     (extract data))))

; Output

(defn no-overwrite [path func]
  (let [file (io/file path)]
    (if (.exists file)
      (throw (ex-info (str "File exists: " path) {:path path}))
      (func path))))

(defn pprint-with-meta [x]
  (let [orig-dispatch pprint/*print-pprint-dispatch*]
    (pprint/with-pprint-dispatch
      (fn [y]
        (when (meta y)
          (print "^")
          (orig-dispatch (meta y))
          (pprint/pprint-newline :mandatory))
        (orig-dispatch y))
      (pprint/pprint x))))

(defn safe-spit-edn
  "Pretty print content (including metadata) to file unless file exists."
  [file content]
  (no-overwrite file (fn [path] (spit path (with-out-str (pprint-with-meta content))))))

(defn safe-spit
  "Print content to file unless file exists."
  [file content]
  (no-overwrite file (fn [path] (spit path content))))

(defn write-csv-rows
  "Write list of (similarly-keyed) maps to writer. Not every keyword has to be in every map."
  [writer rows]
  (let [columns (->> rows (mapcat keys) set sort)
        kws? (every? keyword? columns)
        headers (if kws? (map name columns) columns)]
    (csv/write-csv writer
                   (concat [headers] (for [row rows] (for [col columns] (get row col)))))))

(defn rows->csv
  [file rows]
  {:pre [(s/ends-with? file ".csv")]}
  (with-open [writer (io/writer file)]
    (write-csv-rows writer rows)))

(defn safe-csv "Save list of similarly-keyed maps to filename.csv unless it exists."
  [file rows] (no-overwrite file (fn [path] (rows->csv path rows))))
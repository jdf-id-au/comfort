(ns comfort.core
  "All in one namespace for the time being."
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pprint])
  (:import (java.nio.file FileSystems)
           (java.io File)))

; Files

(defn ext
  "Find files of a particular extension in dirname.
   Map basenames (modified by namer) to `File`s."
  ([extension dirname] (ext extension keyword dirname))
  ([extension namer dirname]
   (into (sorted-map)
         (for [file (.listFiles (io/file dirname))
               :let [filename (.getName file)
                     lcf (s/lower-case filename)]
               :when (s/ends-with? lcf extension)
               :let [name (namer (subs filename 0 (dec (s/last-index-of lcf extension))))]]
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
    ([file] (csv-row-seq file nil))
    ([file {:keys [namer] :or {namer keyword}}]
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

(defn data-fn
  "Allow string keys to work like keyword keys do on maps."
  [columns]
  (let [kws? (every? keyword? columns)]
    (apply juxt (if kws? columns ; explicit but redundant
                         (map (fn [col] (fn [row] (get row col))) columns)))))

(defn rows->csv
  "Save list of (similarly-keyed) maps to filename.csv. Not every keyword has to be in every map."
  [file rows]
  {:pre [(s/ends-with? file ".csv")]}
  (let [columns (->> rows (map keys) (apply concat) set sort)
        kws? (every? keyword? columns)
        headers (if kws? (map name columns) columns)]
    (with-open [writer (io/writer file)]
      (csv/write-csv writer (concat [headers] (map (data-fn columns) rows))))))

(defn safe-csv "Save list of like-keyed maps to filename.csv unless it exists."
  [file rows] (no-overwrite file (fn [path] (rows->csv path rows))))

; Profiling

(defn mem-report []
  (let [display (fn [bytes name] (format (str name ": %.2f GiB") (/ bytes 1024 1024 1024.)))
        rt (.. Runtime getRuntime)
        free (.freeMemory rt)
        total (.totalMemory rt)
        max (.maxMemory rt)
        used (- total free)]
    (apply str
      (interpose ", "
        (for [[bytes name] [[used "used"]
                            [free "free"]
                            [total "total"]
                            [max "max"]]]
          (display bytes name))))))

(defn gc "Collect garbage." []
  (.. Runtime getRuntime gc))

(defmacro timed
  "Evaluate expr and return a vector of the time it took in seconds and the value of expr."
  [expr]
  `(let [start# (. System nanoTime)
         value# ~expr]
     [(/ (- (. System nanoTime) start#) 1e9)
      value#]))

(defmacro doseq-timed
  "Like doseq except return execution time in seconds."
  [seq-exprs & body]
  `(let [start# (. System nanoTime)]
     (doseq ~seq-exprs ~@body)
     (/ (- (. System nanoTime) start#) 1e9)))
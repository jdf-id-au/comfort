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
  "Pretty print string to file unless file exists."
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

; REPL

(defn make-help
  "Show selected symbols naming vars from given ns or alias, and their arglists and docstrings."
  ; clojure.repl doesn't quite have this; bit like dir-fn
  [m]
  (pprint/print-table
    (for [[ns syms] m
          :let [actual-ns (the-ns (get (ns-aliases *ns*) ns ns))]
          [sym var] (map #(vector % (get (ns-publics actual-ns) %)) syms)
          :when (not= "help" (name sym))
          :let [{:keys [doc arglists]} (meta var)]]
      {"ns" (if (= actual-ns *ns*) "" ns)
       "sym" sym "args" arglists "doc" doc})))

; Profiling

(defn mem-report
  ([] (mem-report :M))
  ([unit]
   (let [units {:K ["KiB" (* 1024.)]
                :M ["MiB" (* 1024. 1024)]
                :G ["GiB" (* 1024. 1024 1024)]}
         [suffix divisor] (units unit)
         display (fn [bytes name] (format (str name ": %.2f " suffix) (/ bytes divisor)))
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
           (display bytes name)))))))

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

(def not-daemon (partial filter #(false? (:daemon %))))
(defn print-threads
  "After https://gist.github.com/DayoOliyide/f353b15563675120e408b6b9f504628a
   `(print-threads nil identity)` for full report."
  ([] (print-threads [:name :state :alive :daemon]))
  ([headers] (print-threads headers not-daemon))
  ([headers pre-fn]
   (let [thread-set (keys (Thread/getAllStackTraces))
         thread-data (mapv bean thread-set)
         headers (or headers (-> thread-data first keys))]
     (clojure.pprint/print-table headers (pre-fn thread-data)))))

(defn print-threads-str [& args]
  (with-out-str (apply print-threads args)))
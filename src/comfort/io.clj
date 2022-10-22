(ns comfort.io
  (:require [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [comfort.core :as cc])
  (:import (java.io File BufferedReader)
           (java.nio.file FileSystems Path)
           (java.util.zip GZIPInputStream GZIPOutputStream)))

;; Files

(defn get-extension
  "Return File or filename's extension (lower case)."
  [file]
  (let [fname (if (string? file) file (.getName file))
        idx (s/last-index-of fname \.)]
    (when idx (s/lower-case (subs fname (inc idx))))))

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

(defn path-matcher
  [root syntax pattern]
  {:pre [(some-> root io/file .isDirectory)
         (string? pattern)]}
  (let [root-path (->> root io/file .getPath)
        full-pattern (str root-path File/separator pattern)
        matcher (.getPathMatcher (FileSystems/getDefault) (str syntax \: full-pattern))
        matches (fn [file] (.matches matcher (.toPath file)))]
    (->> root-path io/file file-seq (filter matches))))

(defn glob
  "Find files matching (possibly subdirectory-matching) glob expression.
   Root path is prepended to glob expression.
   e.g. (glob \"path/to/dir\" \"**/*.csv\")"
  [root pattern]
  (path-matcher root "glob" pattern))

(defn regex
  "Find files matching regex.
   Root path is prepended to regex."
  [root pattern]
  (path-matcher root "regex" pattern))

(defn safe-subpath
  "Return File representing path if it's truly a child of parent.
   Use forward-slashes regardless of platform."
  [parent & path-els]
  (let [absolute-parent (-> parent (Path/of (into-array [""])) .toAbsolutePath .normalize)
        absolute-child (-> parent (Path/of (into-array path-els)) .toAbsolutePath .normalize)
        ret (.toFile absolute-child)]
    (when (and (.startsWith absolute-child absolute-parent) (.isFile ret))
      ret)))

;; Input

(defn drop-bom
  "Remove byte order mark from newly-opened UTF-8 reader."
  ;; Simplified from
  ;; https://commons.apache.org/proper/commons-io///jacoco/org.apache.commons.io.input/BOMInputStream.java.html
  ;; https://commons.apache.org/proper/commons-io///jacoco/org.apache.commons.io/ByteOrderMark.java.html
  [^BufferedReader r]
  (.mark r 1)
  (when (not= 0xfeff (.read r))
    (.reset r))
  r)

(defn csv-file-type [file]
  (condp #(s/ends-with? %2 %1) file
    "csv" :csv "csv.gz" :csvgz))

(defn csv-data
  [reader]
  (with-open [r (drop-bom reader)]
    (doall (->> (csv/read-csv r) (cc/mapmap cc/optional-str)))))

(defn read-csv
  "Eagerly load csv as order-preserving maps of optional strings.
   Use comfort.core/detabulate after if needed. "
  [file]
  (case (csv-file-type file)
    :csv (with-open [r (io/reader file)]
           (csv-data r))
    :csvgz (with-open [i (io/input-stream file)
                       g (GZIPInputStream. i)
                       r (io/reader g)]
             (csv-data r))))

(defn sql-statements
  "Extract sql statements for use by jdbc/execute!
   e.g.
   `(jdbc/with-transaction [tx db-spec]
      (for [stmt (-> \"tables.sql\" slurp sql-statements)]
         (jdbc/execute! tx [stmt])))`"
  [s] (-> s
        (s/replace #"\r" "") ; remove CR
        (s/replace #"\s*--.*" "") ; remove comments
        (s/replace #"(?m)^\n+" "") ; remove blank lines
        (s/split #";\n")))

;; Output

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

(defn write-csv
  "Write rows to csv or csv.gz. Use comfort.core/tabulate first if needed."
  [file rows]
  (case (csv-file-type file)
    :csv (with-open [w (io/writer file)] (csv/write-csv w rows))
    :csvgz (with-open [o (io/output-stream file)
                       g (GZIPOutputStream. o)
                       w (io/writer g)] (csv/write-csv w rows))))

(defn safe-csv
  "Save rows to filename.csv or .csv.gz, unless it exists.
   Use comfort.core/tabulate first if needed."
  [file rows] (no-overwrite file (fn [path] (write-csv path rows))))

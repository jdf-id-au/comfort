(ns comfort.io
  (:require [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [comfort.core :as cc])
  (:import (java.io File BufferedReader)
           (java.nio.file FileSystems Path)
           (java.util.zip GZIPInputStream GZIPOutputStream)
           (java.nio.charset StandardCharsets)
           (java.io ByteArrayOutputStream)
           (java.util HexFormat)
           (java.awt.datatransfer DataFlavor StringSelection)))

(defn get-extension ; ──────────────────────────────────────────────────── Files
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

(defn drop-bom ; ───────────────────────────────────────────────────────── Input
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

(defn hex-value
  "Return int."
  [c]
  (if (Character/isDigit c)
    (-> c int (- (int \0)))
    (-> c Character/toUpperCase int (- (int \A)) (+ 0x0a))))

(defn percent-decode
  [s]
  (let [baos (ByteArrayOutputStream.)]
    (loop [[a b c & r :as s] s]
      (case a \% (if (and (HexFormat/isHexDigit (int b)) (HexFormat/isHexDigit (int c)))
                   (do (.write baos (int (+ (* (hex-value b) 0x10) (hex-value c))))
                       (recur r))
                   nil) ; silence if invalid
            nil (.toString baos StandardCharsets/UTF_8)
            (do (.write baos (int a))
                (recur (rest s)))))))

(defn no-overwrite [path func] ; ──────────────────────────────────────── Output
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

(defn temp-file
  "Actually creates the file. Caller to manage deletion."
  ([prefix suffix] (File/createTempFile prefix suffix))
  ([path prefix suffix] (File/createTempFile prefix suffix (-> path io/file .toPath))))

(defn percent-encode ; stunningly absent from jre (URI unhelpful)
  ;; With thanks to http://www.java2s.com/example/java-utility-method/url-encode/uridecode-string-src-3973f.html
  ;; https://www.ietf.org/rfc/rfc3986.txt
  [s]
  (let [sb (StringBuilder.)
        ok (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")]
      (doseq [c s]
        (if (ok c)
          (.append sb c)
          (doseq [b (.getBytes (str c))
                  :let [upper (bit-and (bit-shift-right b 4) 0xf)
                        lower (bit-and b 0xf)]]
            (.append sb \%)
            (.append sb (Integer/toHexString upper))
            (.append sb (Integer/toHexString lower)))))
      (str sb)))

(defn clipboard [] ; ───────────────────────────────────────────────── Clipboard
  ;; after https://github.com/exupero/clipboard
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn paste
  "Paste from system clipboard."
  []
  {:post [(not (s/blank? %))]}
  (.. (clipboard)
    (getContents nil)
    (getTransferData (DataFlavor/stringFlavor))))

(defn retry-paste
  "Paste from system clipboard, retrying once after delay if IOException."
  [delay-ms]
  (try (paste)
       (catch java.io.IOException e
         (println "Clipboard failure, trying again.")
         (Thread/sleep delay-ms)
         (try (paste)
              (catch Exception e
                (println "Unsuccessful clipboard retry." e))))))

(defn copy!
  "Copy to system clipboard."
  [text]
  (let [selection (StringSelection. text)]
    (.setContents (clipboard) selection selection)))

#_(defn flavours []
  (->> (clipboard) .getAvailableDataFlavors (map str)))

#_(defn reader-flavour []
  (->> (clipboard) .getAvailableDataFlavors
    (filter #(= java.io.Reader (.getRepresentationClass %)))
    first))

#_(defn encoding
  "Expecting UTF-16. Java also knows about windows-1252."
  []
  (try (.. (clipboard)
         (getContents nil)
         (getTransferData (reader-flavour))
         (getEncoding))
       (catch java.lang.NullPointerException e
         (println "Nothing in clipboard."))))

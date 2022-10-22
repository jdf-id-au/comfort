(ns comfort.clip
  (:import (java.awt.datatransfer DataFlavor StringSelection Transferable)))

;; after https://github.com/exupero/clipboard

(defn clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn paste
  "Paste from system clipboard."
  []
  (try (.. (clipboard)
      (getContents nil)
      (getTransferData (DataFlavor/stringFlavor)))
    (catch java.lang.NullPointerException e
      (println "Nothing in clipboard."))))

(defn copy!
  "Copy to system clipboard."
  [text]
  (let [selection (StringSelection. text)]
    (.setContents (clipboard) selection selection)))

;; Redundant, just for interest ------------------------------------------------

(defn flavours []
  (->> (clipboard) .getAvailableDataFlavors (map str)))

(defn reader-flavour []
  (->> (clipboard) .getAvailableDataFlavors
    (filter #(= java.io.Reader (.getRepresentationClass %)))
    first))

(defn encoding
  "Expecting UTF-16. Java also knows about windows-1252."
  []
  (try (.. (clipboard)
         (getContents nil)
         (getTransferData (reader-flavour))
         (getEncoding))
       (catch java.lang.NullPointerException e
         (println "Nothing in clipboard."))))

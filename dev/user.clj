(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [comfort.api :as c]))

(def words
  (->> "dev/alice.txt"
       slurp
       (re-find #"(?s)\*\*\* START.*?\*\*\*(.*?)\*\*\* END")
       second
       (#(-> % str/lower-case
             (str/replace #"[^a-z’-]" " ") ; sneaky curly apostrophe
             (str/replace #"--" " ")
             (str/replace #"’" "'")
             (str/split #"\s+")))
       (map #(->> % (re-matches #"['-]*([a-z'-]*?)['-]*") second))
       (filter (comp pos? count))
       set))

#_ (c/safe-spit "resources/words.txt" (->> words (interpose \newline) (apply str)))
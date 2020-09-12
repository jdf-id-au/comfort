(ns comfort.gen
  (:require #?@(:clj  [[clojure.spec.alpha :as s]
                       [clojure.spec.gen.alpha :as gen]
                       [clojure.java.io :as io]]
                :cljs [[cljs.spec.alpha :as s]
                       [cljs.spec.gen.alpha :as gen]
                       [clojure.test.check.generators]])
            [clojure.string :as str])
  #?(:cljs (:require-macros [comfort.gen :refer [dict]]))
  (:refer-clojure :exclude [name comment]))

#?(:clj (def words (-> "words.txt" io/resource slurp str/split-lines)))
#?(:clj (defmacro dict [] (set `~words)))
(defn word "Return word generator." [] (s/gen (dict)))
(defn salad "Return word salad generator." []
  (gen/fmap (fn [ws] (->> ws (interpose " ") (apply str)))
            (s/gen (s/coll-of ::word))))

(defn retag
  "Allow proper generation of multi-spec data.
   Tag is really the dispatch-tag, not the assigned tag,
   so ignore it rather than (assoc gen-v :state-etc tag)."
  [gen-v tag] gen-v)

(defn one [spec] (gen/generate (s/gen spec)))
(defn n [spec n] (gen/sample (s/gen spec) n))
(def exercise s/exercise)
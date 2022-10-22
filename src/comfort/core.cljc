(ns comfort.core
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as str])
  #?(:cljs (:require-macros [comfort.core :refer [ngre]])))

(defn group-by-key
  "Like `group-by`, but groups map values according to a function of their key."
  [f m]
  (persistent!
    (reduce
      (fn [ret [k v]]
        (let [k' (f k)]
          (assoc! ret k' (conj (get ret k' []) v))))
      (transient {}) m)))

(defn update-if-present
  "Update key (if present in item) using function (which could be a map)."
  [item k f]
  {:pre [item k f]}
  (if (contains? item k)
    (update item k f)
    item))

(defn assoc-if-absent
  "Assoc key if absent in item."
  [item k v]
  {:pre [item k v]}
  (if (contains? item k)
    item
    (assoc item k v)))

(defn only
  "Require argument to be coll of one item and return that item."
  [[i & r]]
  {:pre [(nil? r)]}
  i)

(defn unique-wrt
  "Return function which checks whether items' values at key are unique."
  [key] (fn [items] (or (empty? items) (apply distinct? (map key items)))))

(defn distinct-keys?
  "Check whether keys are distinct across maps."
  [& ms]
  (empty? (apply set/intersection (map (comp set keys) ms))))

(defn optional-str
  "Represent both blank string and nil as nil (and therefore null in database).
   Also trims string input."
  [s] (if (str/blank? s) nil (str/trim s)))

(defn mapmap
  "Map f over each coll within c." ; TODO transducer version (think about it)
  [f c]
  (map #(map f %) c))

(defn tabulate
  "Convert seq of similarly-keyed maps to vec containing header then unqualified rows.
   `namer` transforms keys to column header names.
   Not every key has to be in every map."
  ([rows] (tabulate identity rows))
  ([namer rows]
   (let [columns (sort (into #{} (mapcat keys) rows))
         headers (into [] (map namer columns))]
     (into [headers] (for [row rows] (into [] (for [col columns] (get row col))))))))

(defn detabulate
  "Convert seq of header and unqualified rows into order-preserving maps.
   `namer` transforms column header names to keys. Last indistinctly named column wins."
  ([data] (detabulate identity data))
  ([namer data]
   (let [[header & rows] data
         fieldnames (map namer header)
         mapify (fn [row] (apply array-map (interleave fieldnames row)))]
     (map mapify rows))))

(defn without-nil-vals
  "Not recursive."
  [m]
  (into {} (remove (comp nil? second)) m))

(defn redact-keys
  "Set all named keys at any depth in nested map to nil."
  [m & ks]
  (walk/prewalk
    (fn [node]
      (if (map? node)
        (reduce (fn [n k] (update-if-present n k (constantly nil)))
          node ks)
        node)) m))

(defn briefly
  ([clip comment] (cond (nil? comment) nil
                        (<= (count comment) clip) comment
                        :else (str (subs comment 0 clip) "...")))
  ([comment] (briefly 20 comment)))

#?(:clj (defmacro ngre
          "Named group regular expression: define both a regular expression <name>
           and a function <name>-parts calling re-matches which names the found groups."
          [name parts re]
          (let [re-name# name
                matcher# (str name "-parts")]
            `(do (def ~(symbol re-name#) ~re)
                 (defn ~(symbol matcher#) [~'s]
                   (zipmap ~parts (rest (re-matches ~re ~'s))))))))

(defn register
  "Create order-preserving array-map of id->record using ->RecordType factory with vectors of values. Record must include :id field."
  [factory & values]
  (let [records (map #(apply factory %) values)]
    (apply array-map (interleave (map :id records) records))))

(defn debug
  "Tap and pass through value, optionally with message and optionally running function on tapped value.
   Use `clojure.core/add-tap` to see values."
  ([pass-through] (debug nil nil pass-through))
  ([msg pass-through] (debug msg nil pass-through))
  ([msg f pass-through]
   (if f
     (if msg
       (tap> [msg (f pass-through)])
       (tap> (f pass-through)))
     (if msg
       (tap> [msg pass-through])
       (tap> pass-through)))
   pass-through))

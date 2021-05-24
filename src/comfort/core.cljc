(ns comfort.core
  (:require [clojure.walk :as walk])
  #?(:cljs (:require-macros [comfort.core :refer [ngre]])))

; Processing

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
  {:pre [item k f] :post [%]}
  (if (contains? item k)
    (update item k f)
    item))

(defn unique-wrt
  "Return function which checks whether items' values at key are unique."
  [key] (fn [items] (or (empty? items) (apply distinct? (map key items)))))

(defn hierarchicalise
  "Return fully nested vector tree expanded hierarchy by key segment
   when presented with seq of [key value],
   when key is seqable (else treat key as seq of one item)."
  ; NB Cumbersome to interpret if key segs or vals are themselves vectors.
  [kvs]
  (reduce
    (fn across-kvs [acc [k v]]
      (loop [acc-lev acc ; conj to end of vector
             [kf & kr] (if (seqable? k) k [k])
             up nil] ; conj to start of list
        (if kf
          (if-let [sub-lev
                   (->> acc-lev
                        (filter #(and (vector? %) (= kf (first %))))
                        first)]
            (recur sub-lev kr (conj up (into [] (remove #(= sub-lev %)) acc-lev)))
            ; make new level
            (recur (if kr [kf] [kf v]) kr (conj up acc-lev)))
          (reduce
            (fn up-acc [innermost next-out]
              (vec (conj next-out innermost)))
            (conj up acc-lev)))))
    []
    kvs))

(defn map-hierarchicalise
  "Like hierarchicalise, but with nested maps.
   Leaves assoc'd at leaf-key to allow leaves not at tip of branch."
  [kvs leaf-key]
  (reduce
    (fn across-kvs [acc [k v]]
      (update-in acc (if (seqable? k) k [k])
        #(assoc % leaf-key v))) ; make hashmap if node is nil
    {}
    kvs))

(defn optional-str
  "Represent both blank string and nil as nil (and therefore null in database).
   Also trims string input."
  [s] (if (clojure.string/blank? s) nil (clojure.string/trim s)))

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
  "Create order-retaining array-map of id->record using ->RecordType factory with vectors of values. Record must include :id field."
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
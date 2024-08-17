(ns comfort.core
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as str])
  #?(:cljs (:require-macros [comfort.core :refer [ngre]])))

;; ──────────────────────────────────────────────────────────────────────── Text
(defn optional-str
  "Represent both blank string and nil as nil (and therefore null in database).
   Also trims string input."
  [s] (if (str/blank? s) nil (str/trim s)))

(defn briefly
  ([clip comment] (cond (nil? comment) nil
                        (<= (count comment) clip) comment
                        :else (str (subs comment 0 clip) "...")))
  ([comment] (briefly 20 comment)))

#?(:clj (defmacro defre
          "Named group regular expression: define a function `<name>-parts` calling re-matches which names the found groups using `parts`.
  Also define a var `<name>` containing `re`."
          ;; for more sophisticated options, see:
          ;; https://stackoverflow.com/questions/25892277/clojure-regex-named-groups
          ;; TODO cljs impl
          {:clj-kondo/ignore [:unresolved-symbol]}
          [name parts re]
          (assert (symbol? name))
          (assert (vector? parts))
          (assert (instance? java.util.regex.Pattern re))
          `(do
             (def ~name ~re)
             (defn ~(symbol (str name "-parts")) [~'s]
               (some->> ~'s (re-matches ~re) next
                 (zipmap ~(mapv keyword parts)))))))


(defn no-slashes? [s]
  (if s (not (str/includes? s "/"))
      true))

(defn kw->str
  "Convert possibly-namespaced keyword to string."
  [k]
  (when k
    (assert (keyword? k))
    (let [ns (namespace k) n (name k)]
      (assert (no-slashes? ns))
      (assert (no-slashes? n))
      (cond->> n ns (str ns \/)))))

(defn str->kw
  "Convert possibly-'namespaced' string to keyword."
  [s]
  (when-not (str/blank? s)
    (assert (->> s seq (filter #(= \/ %)) count (> 2)))
    (keyword s)))

(declare mapmap)
(defn print-table
  "Doesn't currently support ragged tables. Better for multiline vals than clojure.pprint/print-table."
  ;; entirely unoptimised
  [& rows]
  {:pre [(apply = (map count rows))]}
  (let [rcl (mapmap (fnil str/split-lines "") rows) ; nested rows->cols->lines
        widest-line (fn [lines] (->> lines (map count) (apply max)))
        widths (apply map ; i.e. all first cols then all second cols
                 (fn [& cols] (->> cols (map widest-line) (apply max 1)))
                 rcl)
        heights (map
                  (fn [cols] (->> cols (map count) (apply max 1)))
                  rcl)
        [TL T TT TR ; top left, top (plain), top tick, top right
         L LT R RT
         H V HT
         BL B BT BR] (seq "┌─┬┐│├│┤─│┼└─┴┘")]
    (->>
      (concat
        [(loop [[w & r] widths
                acc [TL]]
           (if r
             (recur r (concat acc (repeat w T) [TT]))
             (apply str (concat acc (repeat w T) [TR]))))]
        (for [[r [h row]] (map-indexed vector (map vector heights rcl))
              l (range (inc h))
              :when (not (and (= l h) (= (inc r) (count rcl))))]
          (str
            (if (= l h) LT L)
            (->>
              (for [[w col] (map vector widths row)
                    :let [v (if (= l h)
                              (apply str (repeat w H))
                              (get col l))
                          n (count v)]
                    ]
                (apply str v (repeat (- w n) \space)))
              (interpose (if (= l h) HT V))
              (apply str))
            (if (= l h) RT R)))
        [(loop [[w & r] widths
                acc [BL]]
           (if r
             (recur r (concat acc (repeat w B) [BT]))
             (apply str (concat acc (repeat w B) [BR]))))])
      (map println) dorun)))

(defmacro print-table-with
  "Print table of unevaluated xs then the values of (f x).
  Useful for narrow multiline string values."
  [f & xs]
  `(print-table ~(mapv str xs) (map ~f ~(vec xs))))

;; ───────────────────────────────────────────────────────────────── Collections
(defn mapmap
  "Map f over each coll within c." ; TODO transducer version (think about it)
  [f c]
  (map #(map f %) c))

(defn only
  "Require argument to be coll of one item and return that item."
  [[i & r]]
  {:pre [(nil? r)]}
  i)

(defn collate-by
  "Use to reduce a coll into a map of key -> coll of vals (default sorted-set).
  Like group-by but less voluminous." ; TODO could impl with transient
  ([keyfn valfn] (collate-by keyfn valfn (sorted-set)))
  ([keyfn valfn into-coll]
   (fn [acc x] (update acc (keyfn x) (fnil #(conj % (valfn x)) into-coll)))))

;; ──────────────────────────────────────────────────────────────────────── Maps
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
  {:pre [k f]}
  (if (contains? item k)
    (update item k f)
    item))

(defn assoc-if-absent
  "Assoc key if absent in item."
  [item k v]
  {:pre [k v]}
  (if (contains? item k)
    item
    (assoc item k v)))

(defn unique-wrt
  "Return function which checks whether items' values at key are unique."
  [key] (fn [items] (or (empty? items) (apply distinct? (map key items)))))

(defn distinct-keys?
  "Check whether keys are distinct across maps."
  [& ms]
  (empty? (apply set/intersection (map (comp set keys) ms))))

(defn without-nil-vals
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

(defn register
  "Create order-preserving array-map of id->record using ->RecordType factory with vectors of values. Record must include :id field."
  [factory & values]
  (let [records (map #(apply factory %) values)]
    (apply array-map (interleave (map :id records) records))))

;; ────────────────────────────────────────────────────────────────────── Tables
(defn column-order
  [preferred actual]
  (let [specified (set preferred)
        actual (set actual)
        unspecified (set/difference actual specified)
        missing (set/difference specified actual)]
    (into (filterv (complement missing) preferred) (sort unspecified))))

(defn tabulate
  "Convert seq of similarly-keyed maps to vec containing header then unqualified rows.
   `namer` transforms keys to column header names.
   Not every key has to be in every map.
   `colseq` specifies preferred column order (prior to application of `namer`).
   Unspecified columns are sorted and appended, specified-but-missing columns are ignored.
   "
  ([rows] (tabulate identity rows))
  ([namer rows]
   (let [columns (sort (into #{} (mapcat keys) rows))
         headers (into [] (map namer columns))]
     (into [headers] (for [row rows] (into [] (for [col columns] (get row col)))))))
  ([colseq namer rows]
   (let [columns (column-order colseq (into #{} (mapcat keys) rows))]
     (into [(map namer columns)]
       (for [row rows] (into [] (for [col columns] (get row col))))))))

(defn detabulate
  "Convert seq of header and unqualified rows into order-preserving maps.
   `namer` transforms column header names to keys. Last indistinctly keyed column wins."
  ([data] (detabulate identity data))
  ([namer data]
   (let [[header & rows] data
         fieldnames (map namer header)
         mapify (fn [row] (apply array-map (interleave fieldnames row)))]
     (map mapify rows))))

;; ────────────────────────────────────────────────────────────────────── Graphs
(defn hierarchicalise
  "Reducer of seq of [key value]
   into []:
   returns order-preserving vector tree hierarchy by key segment
   when key is seqable (else treat key as seq of one item);
   or into {}:
   returns non-order-preserving nested map by key segment
   assoc'ing values at ::leaf to allow branch to grow beyond leaf."
  ; NB Cumbersome to interpret vector tree if key segs or vals are themselves vectors.
  [acc [k v]]
  (cond
    (vector? acc)
    (loop [acc-lev acc ; conj to end of vector
           [kf & kn] (if (seqable? k) k [k])
           up nil] ; conj to start of list
      (if kf
        (if-let [sub-lev
                 (->> acc-lev
                      (filter #(and (vector? %) (= kf (first %))))
                      first)]
          (recur (if kn sub-lev (conj sub-lev v))
            kn (conj up (into [] (remove #(= sub-lev %)) acc-lev)))
          ; make new level
          (recur (if kn [kf] [kf v]) kn (conj up acc-lev)))
        (reduce
          (fn up-acc [innermost next-out]
            (vec (conj next-out innermost)))
          (conj up acc-lev))))
    (map? acc)
    (update-in acc (if (seqable? k) k [k])
      #(assoc % ::leaf v)))) ; make hashmap if node is nil

;; for reference https://groups.google.com/g/clojure/c/h1m6Qjuh3wA/m/pRqNY5HlYJEJ

(defn add-node-id
  [graph id]
  (if (graph id)
    graph
    (assoc graph id {:next #{} :prev #{}})))

(defn add-edge
  [graph from-id to-id]
  (-> graph
    (add-node-id from-id)
    (add-node-id to-id)
    (update-in [from-id :next] conj to-id)
    (update-in [to-id :prev] conj from-id)))

(defn graph
  "Use to reduce colls of nodes into map of {node-id {:prev #{node-id} :next #{node-id}}}.
   from-id = to-id only adds node-id, not an edge. "
  [graph [from-id to-id :as node]]
  (if (and from-id to-id)
    (if (= from-id to-id) ; strictly this is a cycle, but is elided
      (add-node-id graph from-id)
      (add-edge graph from-id to-id))
    (do
      (println "Skipped node (need both from-id and to-id):" node)
      graph)))

(defn dag-impl
  "Cycle at root will return empty map." ; FIXME?
  ([graph] (into {} (for [[node-id {:keys [prev]}] graph
                          :when (empty? prev)]
                      [node-id (dag-impl node-id graph '())])))
  ([node-id graph path]
   (let [seen (set path)
         proposed (conj path node-id)]
     (if (seen node-id)
       ::cycle-detected
       (some->>
         (for [child (get-in graph [node-id :next])]
           [child (dag-impl child graph proposed)])
         seq (into {}))))))

(defn dag [nodes]
  (->> nodes (reduce graph {}) dag-impl))

(defn deps-order
  "List ids from nodes of [from-id to-id] such that no id depends
  on one which appears later in list."
  [nodes]
  (let [dag (dag nodes)
        queue (loop [[k & r :as queue] nil
                     [[dk dv] & dr :as deps] dag]
                (if dv
                  (case dv
                    ::cycle-detected (throw (ex-info "cycle detected"
                                              {:at dk}))
                    (recur (conj queue dk) (concat dv dr)))
                  (if dk
                    (recur (conj queue dk) dr)
                    queue)))]
    (distinct queue)))

#?(:clj
   (defmacro with-resource ; like with-open TODO clj-kondo hooks
     "bindings => [name init deinit ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (deinit name) on
  each name in reverse order."
     {:clj-kondo/ignore [:unresolved-symbol]}
     [bindings & body]
     (assert (vector? bindings))
     (assert (zero? (mod (count bindings) 3)))
     (cond
       (= (count bindings) 0) `(do ~@body)
       (symbol? (bindings 0)) `(let ~(subvec bindings 0 2) 
                                 (try
                                   (with-resource ~(subvec bindings 3) ~@body)
                                   (finally
                                     (when-let [deinit# ~(bindings 2)]
                                       (deinit# ~(bindings 0))))))
       :else (throw (IllegalArgumentException.
                      "with-resource only allows Symbols in bindings"))))
   )

;; ───────────────────────────────────────────────────────────────────────── Dev
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

#?(:clj
   (defn idef
     "Wraps intern for easier docstring."
     ([ns sym doc val]
      (intern ns (with-meta sym {:doc doc}) val))
     ([qsym doc val]
      (idef (-> qsym namespace symbol) (-> qsym name symbol) doc val))))

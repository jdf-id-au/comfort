(ns comfort.plot
  "Data manipulation to prepare plot. Not directly graphical."
  (:import (java.time LocalDate LocalTime LocalDateTime)
           (java.time.temporal ChronoUnit))
  (:refer-clojure :exclude [range]))

(def clj-range clojure.core/range)

(def ldt-epoch "Oddly missing from JDK vs LocalDate/EPOCH"
  (LocalDateTime/of LocalDate/EPOCH LocalTime/MIDNIGHT))
(def ld->n #(.until LocalDate/EPOCH % ChronoUnit/DAYS))
(def n->ld #(LocalDate/ofEpochDay %))
(def lt->n #(.until LocalTime/MIDNIGHT % ChronoUnit/SECONDS))
(def n->lt #(.plusSeconds LocalTime/MIDNIGHT %))
(def ldt->n #(.until ldt-epoch % ChronoUnit/SECONDS))
(def n->ldt #(.plusSeconds ldt-epoch %))

#_(do (ns-unmap *ns* 'domain-fn)
      (ns-unmap *ns* 'range-fn)) ; make reload-ns fresh

;; https://en.wikipedia.org/wiki/Expression_problem
(defmulti domain-fn
  "Return fn converting values in range [from to] or [seq] to number
  (could be ratio etc)."
  (fn [& [from to]] (if to (type from) :seq)))
(defmethod domain-fn LocalDateTime [& [from to]]
  (let [[f t] (map ldt->n [from to])]
    (fn ldtd [x] (/ (- (ldt->n x) f) (- t f)))))
(defmethod domain-fn LocalTime [& [from to]]
  (let [[f t] (map lt->n [from to])]
    (fn ldtd [x] (/ (- (lt->n x) f) (- t f)))))
(defmethod domain-fn LocalDate [& [from to]]
  (let [[f t] (map ld->n [from to])]
    (fn ldd [x] (/ (- (ld->n x) f) (- t f)))))
(defmethod domain-fn :seq [& [coll]]
  (let [lookup (zipmap coll (clj-range))
        n (dec (count coll))]
    (fn sd [x] (/ (lookup x) n))))
(defmethod domain-fn :default [& [from to]]
  (fn dd [x] (/ ( - x from) (- to from))))

(defmulti range-fn
  "Return fn converting numbers from domain-fn to values in range
  [from to] or [seq]."
  (fn [& [from to]] (if to (type from) :seq)))
(defmethod range-fn LocalDateTime [& [from to]]
  (let [[f t] (map ldt->n [from to])]
    (fn ldtr [x] (n->ldt (+ f (* (- t f) x))))))
(defmethod range-fn LocalTime [& [from to]]
  (let [[f t] (map lt->n [from to])]
    (fn ldtr [x] (n->lt (+ f (* (- t f) x))))))
(defmethod range-fn LocalDate [& [from to]]
  (let [[f t] (map ld->n [from to])]
    (fn ldr [x] (n->ld (+ f (* (- t f) x))))))
(defmethod range-fn :seq [& [coll]]
  (let [lookup (zipmap (clj-range) coll)
        n (dec (count coll))]
    (fn sr [x] (lookup (* x n)))))
(defmethod range-fn :default [& [from to]]
  (fn dr [x] (+ from (* (- to from) x))))

#_(ns-unmap *ns* 'ops)
(defmulti ops
  "Because defprotocol intentionally doesn't have default case."
  type)
(defmethod ops LocalDateTime [_]
  {'min #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) a b))) %&)
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)
   '+ #(.plusSeconds %1 %2) ; other arities not supported
   '- (fn [ldt x]
        (if (instance? LocalDateTime x)
          (.until x ldt ChronoUnit/SECONDS)
          (.plusSeconds ldt (- x))))})
(defmethod ops LocalTime [_]
  {'min #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) a b))) %&)
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)
   '+ #(.plusSeconds %1 %2)
   '- (fn [ldt x]
        (if (instance? LocalTime x)
          (.until x ldt ChronoUnit/SECONDS)
          (.plusSeconds ldt (- x))))})
(defmethod ops LocalDate [_]
  {'min #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) a b))) %&)
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)
   '+ #(.plusDays %1 %2)
   '- (fn [ldt x]
        (if (instance? LocalDate x)
          (.until x ldt ChronoUnit/DAYS)
          (.plusDays ldt (- x))))})
(defmethod ops :default [_]
  {'min min 'max max '+ + '- -})

(defn normalise
  [vals]
  (let [maximum (apply max (map abs vals))]
    (map #(double (/ % maximum)) vals)))

(defn normalise-all
  "Expects [[x0 y0 ...] [x1 y1 ...] ...]. Scales to xmax=±1.0 etc."
  [rows]
  (let [max-abs (fn [& xs] (apply max (map abs xs)))
        maxima (reduce (fn ([] nil) ([a b] (map max-abs a b))) rows)]
    (mapv (fn [row] (mapv (comp double /) row maxima)) rows)))

(defn span
  [vals]
  (let [{:syms [min max]} (-> vals first ops)]
    (reduce (fn [[mn mx] x]
              [(if mn (min x mn) x)
               (if mx (max x mx) x)])
      nil vals)))

;; Tick implementation cribbed from d3
;; https://github.com/d3/d3/blob/main/LICENSE

(defn tick-spec
  [start stop count]
  (let [e10 (Math/sqrt 50)
        e5 (Math/sqrt 10)
        e2 (Math/sqrt 2)
        step (/ (- stop start) (max 0 count))
        power (Math/floor (Math/log10 step))
        error (/ step (Math/pow 10 power))
        factor (cond
                 (>= error e10) 10
                 (>= error e5) 5
                 (>= error e2) 2
                 :else 1)
        ;; spelling d3's `inc` as `incr`
        return (fn [i1 i2 incr]
                 (if (and (< i2 i1) (<= 0.5 count) (< count 2))
                   (tick-spec start stop (* 2 count))
                   [i1 i2 incr]))]
    (if (< power 0)
      (let [incr (/ (Math/pow 10 (- power)) factor)
            i1 (Math/round (* start incr))
            i2 (Math/round (* stop incr))
            i1 (cond-> i1 (< (/ i1 incr) start) inc)
            i2 (cond-> i2 (> (/ i2 incr) stop) dec)
            incr (- incr)]
        (return i1 i2 incr))
      (let [incr (* (Math/pow 10 power) factor)
            i1 (Math/round (/ start incr))
            i2 (Math/round (/ stop incr))
            i1 (cond-> i1 (< (* i1 incr) start) inc)
            i2 (cond-> i2 (> (* i2 incr) stop) dec)]
        (return i1 i2 incr)))))

(defn ticks [start stop count]
  (cond
    (<= count 0) []
    (= start stop) [start]
    :else
    (let [reverse? (< stop start)
          [i1 i2 incr] (if reverse?
                         (tick-spec stop start count)
                         (tick-spec start stop count))
          n (inc (- i2 i1))]
      (if (< i2 i1) []
        (if reverse?
          (if (neg? incr)
            (for [i (clj-range n)] (/ (- i2 i) (- incr)))
            (for [i (clj-range n)] (* (- i2 i) incr)))
          (if (neg? incr)
            (for [i (clj-range n)] (/ (+ i1 i) (- incr)))
            (for [i (clj-range n)] (* (+ i1 i) incr))))))))

(defn tick-increment [start stop count]
  (nth (tick-spec start stop count) 2))

(defn nice
  ([start end])
  ([start end {:keys [::nice]}]
   (let [{:syms [+ -]} (ops start)
         width (- end start)]
     )))

(defn range
  ([])
  ([end]) ; TODO what would start be for LocalDate etc?
  ([start end]
   (range start end 1))
  ([start end step]
   (let [{:syms [+ -]} (ops start)
         [start end step]
         (cond
           (number? step) [start end step]
           (= ::nice step) (nice start end)
           (map? step) (nice start end step))]
     (for [n (clj-range (/ (- end start) step))]
       (+ start (* n step))))))

(defn ceil-div [a b]
  (Math/ceil (/ a b)))

(defn centred-points
  [w h n]
  (when (pos? n)
    (let [aspect-ratio (/ w h)
          rows (ceil-div n (* (Math/sqrt n) aspect-ratio))
          cols (ceil-div n rows)
          last-line (rem n cols)
          cx (/ w 2) ; centre of area
          cy (/ h 2)
          dx (/ w cols) ; distance between points
          dy (/ h rows)
          d (min dx dy)
          gw (* cols d) ; point group
          gh (* rows d)
          gx (- cx (/ gw 2))
          gy (- cy (/ gh 2))]
      (for [c (clj-range cols) r (clj-range rows)
            :while (<= (+ (* r cols) (inc c)) n)]
        [(+ gx (* (+ c 0.5) d)) (+ gy (* (+ r 0.5) d))]))))

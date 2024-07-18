(ns comfort.plot
  (:import (java.time LocalDate LocalTime LocalDateTime)
           (java.time.temporal ChronoUnit)))

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
  (let [[f t] (map ldt->n [from to])] ; TODO follow through nanos...
    (fn ldtd [x] (/ (- (ldt->n x) f) (- t f)))))
(defmethod domain-fn LocalTime [& [from to]]
  (let [[f t] (map lt->n [from to])] ; TODO follow through nanos...
    (fn ldtd [x] (/ (- (lt->n x) f) (- t f)))))
(defmethod domain-fn LocalDate [& [from to]]
  (let [[f t] (map ld->n [from to])]
    (fn ldd [x] (/ (- (ld->n x) f) (- t f)))))
(defmethod domain-fn :seq [& [coll]]
  (let [lookup (zipmap coll (range))
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
  (let [lookup (zipmap (range) coll)
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
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)})
(defmethod ops LocalTime [_]
  {'min #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) a b))) %&)
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)})
(defmethod ops LocalDate [_]
  {'min #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) a b))) %&)
   'max #(reduce (fn ([]) ([a b] (if (neg? (.compareTo a b)) b a))) %&)})
(defmethod ops :default [_]
  {'min min 'max max})

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
      (for [c (range cols) r (range rows)
            :while (<= (+ (* r cols) (inc c)) n)]
        [(+ gx (* (+ c 0.5) d)) (+ gy (* (+ r 0.5) d))]))))

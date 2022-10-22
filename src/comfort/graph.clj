(ns comfort.graph)

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

(defn graph-by
  "Return fn which can be used to reduce colls of nodes into map of {node-id {:prev #{node-id} :next #{node-id}}}.
   from-id = to-id only add node-id, not an edge.
   edge-fn needs to return [from-id to-id]"
  [edge-fn]
  (fn [graph node]
    (let [[from-id to-id] (edge-fn node)]
      (if (and from-id to-id)
        (if (= from-id to-id)
          (add-node-id graph from-id)
          (add-edge graph from-id to-id))
        (do
          (println "Skipped node (need both from-id and to-id):" node)
          graph)))))

(defn dag
  "Cycle at root will return empty map."
  ([graph] (into {} (for [[node-id {:keys [prev]}] graph
                          :when (empty? prev)]
                      [node-id (dag node-id graph '())])))
  ([node-id graph path]
   (let [seen (set path)
         proposed (conj path node-id)]
     (if (seen node-id)
       (conj proposed ::cycle-detected)
       (into {}
         (for [child (get-in graph [node-id :next])]
           [child (dag child graph proposed)]))))))

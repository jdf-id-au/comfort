(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

;; ──────────────────────────────────────────────────────────────────────── Text
(deftest ngre
  #?(:clj
     (do (cc/defre ln [letters numbers] #"([a-zA-Z]*)([0-9]*)")
         (is (= #"([a-z][A-Z]*)([0-9]*)" ln))
         (is (= {:letters "abcd" :numbers "0123"}
               (ln-parts "abcd0123"))))))

;; ───────────────────────────────────────────────────────────────── Collections
(deftest collate-by
  (is (= {:a #{:b :c}, :b #{:d :e}, :c #{nil}}
        (reduce (cc/collate-by first second)
          (sorted-map)
          [[:a :b] [:a :c] [:b :d] [:b :e] [:c nil]]))))

;; ────────────────────────────────────────────────────────────────────── Tables
(deftest column-order
  (is (= [:a :c :d :e :f :g]
        (cc/column-order
          [:a :B :b :C :c]
          [:g :f :e :d :c :a]))))

(deftest tabulate
  (is (= [["a" "b" "c" "d"]
          [1 2 3 nil]
          [4 5 6 7]]
        (cc/tabulate name [{:c 3 :a 1 :b 2} {:b 5 :a 4 :c 6 :d 7}])))
  (is (= [["a" "b" "c"]
          [1 2 3]
          [4 5 6]]
        (cc/tabulate [{"c" 3 "a" 1 "b" 2} {"b" 5 "a" 4 "c" 6}]))))

(deftest detabulate
  (is (= '({:a 1 :b 2 :c 3}
           {:a 4 :b 5 :c 6})
        (cc/detabulate [[:a :b :c]
                        [1 2 3]
                        [4 5 6]]))))

;; ────────────────────────────────────────────────────────────────────── Graphs
(deftest dag
  (let [nodes [[0 1] [1 2] [1 3] [1 4] [2 4] [3 1] [2 5]]
        acyclic-nodes [[0 1] [1 2] [1 3] [1 4] [2 4] [2 5] [1 6]]
        graph (->> nodes (reduce cc/graph {}))]
    (is (= graph {0 {:next #{1} :prev #{}}
                  1 {:next #{2 3 4} :prev #{0 3}}
                  2 {:next #{4 5} :prev #{1}}
                  3 {:next #{1} :prev #{1}}
                  4 {:next #{} :prev #{1 2}}
                  5 {:next #{} :prev #{2}}}))
    (is (= (cc/dag-impl graph)
          {0 {1 {2 {4 nil 5 nil}
                 3 {1 ::cc/cycle-detected}
                 4 nil}}}))
    (is (= (cc/dag acyclic-nodes)
          {0 {1 {2 {4 nil 5 nil}
                 3 nil
                 4 nil
                 6 nil}}}))
    (is (= (cc/deps-order acyclic-nodes)
          '(5 4 2 3 6 1 0)))))

(deftest hierarchicalise
  (is (= [[:a :A
              [:b :B
                  [:c :C]]
              [:d :D]]
          [:z :Z]]
        (reduce cc/hierarchicalise []
          {[:a] :A ; key is seq len 1
           [:a :b] :B
           [:a :b :c] :C
           [:a :d] :D
           :z :Z}))) ; key is not seqable
  (is (= [[:a :A
              [:b [:c :C] ; order preserved
                  :B]
              [:d :D]]
          [:z :Z]]
        (reduce cc/hierarchicalise []
          {[:a] :A
           [:a :b :c] :C ; deeper first
           [:a :b] :B
           [:a :d] :D
           :z :Z})))
  (is (= {:a {::cc/leaf :A
              :b {::cc/leaf :B
                  :c {::cc/leaf :C}}
              :d {::cc/leaf :D}}
          :z {::cc/leaf :Z}}
        (reduce cc/hierarchicalise {}
          {[:a] :A
           [:a :b] :B
           [:a :b :c] :C
           [:a :d] :D
           :z :Z}))))

;; for bidi, want to get to
#_ {:a {"" :A
        :b {"" :B
            :c :C} ; can tolerate {"" :C}
        :d :D}
    :z :Z}
; equiv to
#_ [[:a [["" :A]
         [:b [["" :B]
              [:c :C]]]
         [:d :D]]]
    [:z :Z]]

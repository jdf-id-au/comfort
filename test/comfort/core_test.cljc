(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

(deftest collate-by
  (is (= {:a #{:b :c}, :b #{:d :e}, :c #{nil}}
        (reduce (cc/collate-by first second)
          (sorted-map)
          [[:a :b] [:a :c] [:b :d] [:b :e] [:c nil]]))))

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

(deftest dag
  (let [nodes [[0 1] [1 2] [1 3] [1 4] [2 4] [3 1] [2 5]]
        edge-fn identity
        graph (->> nodes (reduce (cc/graph-by edge-fn) {}))]
    (is (= graph {0 {:next #{1} :prev #{}}
                  1 {:next #{2 3 4} :prev #{0 3}}
                  2 {:next #{4 5} :prev #{1}}
                  3 {:next #{1} :prev #{1}}
                  4 {:next #{} :prev #{1 2}}
                  5 {:next #{} :prev #{2}}}))
    (is (= (cc/dag graph)
          {0 {1 {2 {4 {} 5 {}}
                 3 {1 ::cc/cycle-detected}
                 4 {}}}}))))

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

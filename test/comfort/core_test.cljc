(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

(deftest tabulate
  (is (= [["a" "b" "c"]
          [1 2 3]
          [4 5 6]]
        (cc/tabulate name [{:c 3 :a 1 :b 2} {:b 5 :a 4 :c 6}])))
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

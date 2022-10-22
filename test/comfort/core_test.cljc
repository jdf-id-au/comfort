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

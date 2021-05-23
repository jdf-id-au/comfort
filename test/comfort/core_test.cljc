(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

(deftest hierarchicalise
  (is (= [[:a :A
              [:b :B
                  [:c :C]]
              [:d :D]]
          [:z :Z]]
        (cc/hierarchicalise {[:a] :A
                             [:a :b] :B
                             [:a :b :c] :C
                             [:a :d] :D
                             [:z] :Z}))))

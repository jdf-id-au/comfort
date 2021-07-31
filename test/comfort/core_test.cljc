(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

(deftest tabulate
  (is (= [["a" "b" "c"]
          [1 2 3]
          [4 5 6]]
        (cc/tabulate [{:c 3 :a 1 :b 2} {:b 5 :a 4 :c 6}])))
  (is (= [["a" "b" "c"]
          [1 2 3]
          [4 5 6]]
        (cc/tabulate [{"c" 3 "a" 1 "b" 2} {"b" 5 "a" 4 "c" 6}]))))

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
; for bidi, want to get to
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
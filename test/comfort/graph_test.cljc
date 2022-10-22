(ns comfort.graph-test
  (:require [clojure.test :refer :all]
            [comfort.graph :as cg]))

(deftest hierarchicalise
  (is (= [[:a :A
              [:b :B
                  [:c :C]]
              [:d :D]]
          [:z :Z]]
        (reduce cg/hierarchicalise []
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
        (reduce cg/hierarchicalise []
          {[:a] :A
           [:a :b :c] :C ; deeper first
           [:a :b] :B
           [:a :d] :D
           :z :Z})))
  (is (= {:a {::cg/leaf :A
              :b {::cg/leaf :B
                  :c {::cg/leaf :C}}
              :d {::cg/leaf :D}}
          :z {::cg/leaf :Z}}
        (reduce cg/hierarchicalise {}
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

(deftest dag
  (is (= {:a {:c {:d {}}
              :b {}}}
        (->> [[:a :b] [:c :d] [:a :c]]
          (reduce (cg/graph-by identity) {})
          cg/dag)))
  (is (= {:a {:c {:d {:c '(::cg/cycle-detected :c :d :c :a)}}
              :b {}}}
        (->> [[:a :b] [:c :d] [:a :c] [:d :c]]
          (reduce (cg/graph-by identity) {})
          cg/dag))))

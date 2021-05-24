(ns comfort.core-test
  (:require [clojure.test :refer :all]
            [comfort.core :as cc]))

(deftest hierarchicalise
  (is (= [[:a :A
              [:b :B
                  [:c :C]]
              [:d :D]]
          [:z :Z]]
        (cc/hierarchicalise
          {[:a] :A ; key is seq len 1
           [:a :b] :B
           [:a :b :c] :C
           [:a :d] :D
           :z :Z}))) ; key is not seqable
  (is (= {:a {"" :A
              :b {"" :B
                  :c {"" :C}}
              :d {"" :D}}
          :z {"" :Z}}
        (cc/map-hierarchicalise
          {[:a] :A ; key is seq len 1
           [:a :b] :B
           [:a :b :c] :C
           [:a :d] :D
           :z :Z}
          ""))))
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
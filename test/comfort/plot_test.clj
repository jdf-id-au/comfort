(ns comfort.plot-test
  (:require [clojure.test :refer :all]
            [comfort.plot :as cp
             :refer [domain-fn range-fn]])
  (:refer-clojure :exclude [range]))

#_(do (ns-unmap *ns* 'domain-fn)
      (ns-unmap *ns* 'range-fn)) ; make reload-ns fresh

(deftest scaling
  (is (= 300 (-> 3 ((domain-fn 0 10)) ((range-fn 0 1000)))))
  (is (= 13 (-> 3 ((domain-fn 0 10)) ((range-fn 10 20)))))
  (is (= 17 (-> 3 ((domain-fn 0 10)) ((range-fn 20 10)))) "Inverted range works.")
  (is (= 7 (-> 3 ((domain-fn 10 0)) ((range-fn 0 10)))) "Inverted domain works.")
  (is (= -30 (-> -3 ((domain-fn 0 10)) ((range-fn 0 100)))) "Out of domain works.")
  (is (= 5
        ;; tagged literals from com.widdindustries/time-literals
        (-> #time/date "2020-01-05"
          ((domain-fn #time/date "2020-01-01" #time/date "2020-01-10"))
          ((range-fn 1 10)))))
  (is (= 0.5
        (-> #time/date "2020-01-06"
          ((domain-fn #time/date "2020-01-01" #time/date "2020-01-11"))
          ((range-fn 0.0 1.0)))))
  (is (= 1/2 ; nice!
        (-> #time/date "2020-01-06"
          ((domain-fn #time/date "2020-01-01" #time/date "2020-01-11"))
          ((range-fn 0 1)))))
  (is (= 0.5
        (-> #time/time "06:00:00"
          ((domain-fn #time/time "00:00:00" #time/time "12:00:00"))
          ((range-fn 0.0 1.0)))))
  (is (= 0.5
        (-> #time/time "12:00:00"
          ((domain-fn #time/time "00:00:00" #time/time "00:00:00"))
          ((range-fn 0.0 1.0)))))
  (is (= 0.5
        (-> #time/date-time "2020-01-01T12:00:00"
          ((domain-fn #time/date-time "2020-01-01T00:00:00"
             #time/date-time "2020-01-02T00:00:00"))
          ((range-fn 0.0 1.0)))))
  (is (= 1/2
        (-> #time/date-time "2020-01-01T12:00:00"
          ((domain-fn #time/date-time "2020-01-01T00:00:00"
             #time/date-time "2020-01-02T00:00:00"))
          ((range-fn 0 1)))))
  (is (= #time/date-time "2020-01-01T12:00:00"
        (-> 0.5
          ((domain-fn 0 1))
          ((range-fn #time/date-time "2020-01-01T00:00:00"
             #time/date-time "2020-01-02T00:00:00")))))
  (is (= 2
        (-> :c
          ((domain-fn [:a :b :c :d :e]))
          ((range-fn 0 4)))))
  (is (= :c
        (-> 2
          ((domain-fn 0 4))
          ((range-fn [:a :b :c :d :e])))))
  (is (= :b
        (-> #time/date "2020-01-02"
          ((domain-fn #time/date "2020-01-01"
             #time/date "2020-01-03"))
          ((range-fn [:a :b :c]))))))

(deftest normalise
  (is (= [1.0 -0.2 0.8]
        (cp/normalise [10 -2 8])))
  (is (= [[1.0 0.2 0.8] [1.0 1.0 1.0]]
        (cp/normalise-all [[10 2 80] [10 10 100]])))
  (is (= [[1.0 -0.2 0.8] [1.0 1.0 1.0]]
        (cp/normalise-all [[10 -2 80] [10 10 100]]))))

(deftest range
  (is (= [#time/date "2020-01-01"
          #time/date "2020-01-02"
          #time/date "2020-01-03"]
        (into []
          (cp/range #time/date "2020-01-01" #time/date "2020-01-04" 1))))
  (is (= [#time/date-time "2020-01-01T00:00:00"
          #time/date-time "2020-01-01T01:00:00"
          #time/date-time "2020-01-01T02:00:00"]
        (into []
          (cp/range
            #time/date-time "2020-01-01T00:00:00"
            #time/date-time "2020-01-01T03:00:00" (* 60 60))))))

(deftest ceil-div
  (is (= 3.0 (cp/ceil-div 5 2))))

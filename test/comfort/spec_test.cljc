(ns comfort.spec-test
  (:require [clojure.test :refer :all]
            [comfort.spec :as cs]))

(deftest ngres
  (is (= {:scheme "https" :host "example.com" :port "1234" :path "/path#fragment?query"}
         (cs/URI-parts "https://example.com:1234/path#fragment?query")))
  (is (= {:name "Some One" :username "someone" :host "example.com"}
         (cs/NamedEmail-parts "Some One <someone@example.com>"))))
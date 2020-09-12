(ns comfort.spec
  (:require #?@(:clj  [[clojure.spec.alpha :as s]
                       [clojure.java.io :as io]]
                :cljs [[cljs.spec.alpha :as s]])
            [comfort.gen :as gen]))

; Suitable for use in schema or spec:
(def URI #"^(https?)://([^/:]*):?(\d+)?(/.*)?")
(def Email #"^\S+@\S+\.\S{2,}")
(def NamedEmail #"^[^<>]+ <\S+@\S+\.\S{2,}>")

(s/def ::non-blank-string (s/and string? (comp not clojure.string/blank?)))
(s/def ::word (s/with-gen ::non-blank-string gen/word))
(s/def ::salad (s/with-gen ::non-blank-string gen/salad))

(s/def ::uri (s/and string? #(re-matches URI %))) ; better if conformed to something? or used existing parser?
(s/def ::email (s/and string? #(re-matches Email %)))
(s/def ::named-email (s/and string? #(re-matches NamedEmail %)))
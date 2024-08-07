(ns comfort.spec
  (:require #?@(:clj  [[clojure.spec.alpha :as s]
                       [clojure.java.io :as io]]
                :cljs [[cljs.spec.alpha :as s]])
            [comfort.gen :as gen]
            [comfort.core :as cc]
            [clojure.string :as str]))

(cc/defre URI ; "Match http and https URIs. Any #fragment and ?query remain part of path."
  [scheme host port path] #"^(https?)://([^/:]*):?(\d+)?(/.*)?")
(cc/defre Email [username host] #"(^\S+)@(\S+\.\S{2,})")
(cc/defre NamedEmail [name username host] #"^([^<>]+) <(\S+)@(\S+\.\S{2,})>")

(s/def ::non-blank-string (s/and string? (comp not clojure.string/blank?)))
; ::word allows but doesn't gen containing spaces
(s/def ::word (s/with-gen ::non-blank-string gen/word))
(s/def ::salad (s/with-gen ::non-blank-string gen/salad))

; Consider a macro to make these from re's above...
(s/def ::uri (s/and string? #(re-matches URI %))) ; better if conformed to something? or used existing parser?
(s/def ::email (s/and string? #(re-matches Email %)))
(s/def ::named-email (s/and string? #(re-matches NamedEmail %)))

(ns comfort.spec
  (:require #?@(:clj  [[clojure.spec.alpha :as s]
                       [clojure.java.io :as io]]
                :cljs [[cljs.spec.alpha :as s]])
            [comfort.gen :as gen]
            [comfort.core :as api]))

; Suitable for use in schema or spec.
; Parts metadata is just documentation at the moment:
; cljs doesn't reify vars https://stackoverflow.com/a/50205987/780743 so they're difficult to retrieve.

(api/ngre URI ; "Match http and https URIs. Any #fragment and ?query remain part of path."
  [:scheme :host :port :path] #"^(https?)://([^/:]*):?(\d+)?(/.*)?")
(api/ngre Email [:username :host] #"(^\S+)@(\S+\.\S{2,})")
(api/ngre NamedEmail [:name :username :host] #"^([^<>]+) <(\S+)@(\S+\.\S{2,})>")

(s/def ::non-blank-string (s/and string? (comp not clojure.string/blank?)))
(s/def ::word (s/with-gen ::non-blank-string gen/word))
(s/def ::salad (s/with-gen ::non-blank-string gen/salad))

(s/def ::uri (s/and string? #(re-matches URI %))) ; better if conformed to something? or used existing parser?
(s/def ::email (s/and string? #(re-matches Email %)))
(s/def ::named-email (s/and string? #(re-matches NamedEmail %)))
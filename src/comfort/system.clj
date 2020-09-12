(ns comfort.system
  (:require [clojure.pprint :as pprint])
  (:import (java.net InetAddress)
           (java.lang ProcessHandle)))

; REPL

(defn make-help
  "Show selected symbols naming vars from given ns or alias, and their arglists and docstrings.
   Return hostnames, pid and current ns in case user is disoriented!
   Also see `clojure.repl/dir-fn`.
   Usage:
   user.clj: (def help (partial c/make-help {'ns1 ['var1 ...] ...}))
   repl: (help)"
  [m]
  (pprint/print-table
    (for [[ns syms] m
          :let [actual-ns (the-ns (get (ns-aliases *ns*) ns ns))]
          [sym var] (map #(vector % (get (ns-publics actual-ns) %)) syms)
          :when (not= "help" (name sym))
          :let [{:keys [doc arglists]} (meta var)]]
      {"ns" (if (= actual-ns *ns*) "" ns)
       "sym" sym "args" arglists "doc" doc}))
  (let [localhost (InetAddress/getLocalHost)]
    {:hostname [(.getHostName localhost) (.getCanonicalHostName localhost)]
     :pid (.pid (ProcessHandle/current))
     :ns (ns-name *ns*)}))

; Profiling

(defn mem-report
  ([] (mem-report :M))
  ([unit]
   (let [units {:K ["KiB" (* 1024.)]
                :M ["MiB" (* 1024. 1024)]
                :G ["GiB" (* 1024. 1024 1024)]}
         [suffix divisor] (units unit)
         display (fn [bytes name] (format (str name ": %.2f " suffix) (/ bytes divisor)))
         rt (.. Runtime getRuntime)
         free (.freeMemory rt)
         total (.totalMemory rt)
         max (.maxMemory rt)
         used (- total free)]
     (apply str
       (interpose ", "
         (for [[bytes name] [[used "used"]
                             [free "free"]
                             [total "total"]
                             [max "max"]]]
           (display bytes name)))))))

(defn gc "Collect garbage." []
  (.. Runtime getRuntime gc))

(defmacro timed
  "Evaluate expr and return a vector of the time it took in seconds and the value of expr."
  [expr]
  `(let [start# (. System nanoTime)
         value# ~expr]
     [(/ (- (. System nanoTime) start#) 1e9)
      value#]))

(defmacro doseq-timed
  "Like doseq except return execution time in seconds."
  [seq-exprs & body]
  `(let [start# (. System nanoTime)]
     (doseq ~seq-exprs ~@body)
     (/ (- (. System nanoTime) start#) 1e9)))

(def -not-daemon (partial filter #(false? (:daemon %))))
(defn print-threads
  "`(print-threads nil identity)` for full report."
  ; After https://gist.github.com/DayoOliyide/f353b15563675120e408b6b9f504628a
  ([] (print-threads [:name :state :alive :daemon]))
  ([headers] (print-threads headers -not-daemon))
  ([headers pre-fn]
   (let [thread-set (keys (Thread/getAllStackTraces))
         thread-data (mapv bean thread-set)
         headers (or headers (-> thread-data first keys))]
     (clojure.pprint/print-table headers (pre-fn thread-data)))))

(defn print-threads-str [& args]
  (with-out-str (apply print-threads args)))

; Platform

(def linux? (= "Linux" (System/getProperty "os.name")))
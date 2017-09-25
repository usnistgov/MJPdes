(ns gov.nist.MJPdes.util.utils
  (:require [clojure.pprint :refer (cl-format pprint pp)]))

;;;=== General =========================
(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint *1)))

(defn ppprint [arg]
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint arg)))

(defn break
  ([] (throw (ex-info "Break!" {})))
  ([text] (throw (ex-info text {})))
  ([text args] (throw (ex-info text args))))

(defn mean
  [v]
  (/ (apply + v) (count v)))

(defn variance
  " Var(X) = E[(X - mu)^2]"
  [v]
  (let [avg (mean v)]
    (mean (vec (map (fn [x]
                      (let [dif (- x avg)]
                        (* dif dif)))
                    v)))))

#_(defn analyze-results [filename]
  "Read an output file and perform various calculations."
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (let [results (loop [mp (edn/read {:eof :eof} in)
                         res {:starve []}]
                    (if (not= :eof mp)
                      (recur (edn/read {:eof :eof} in)
                             (update-in res [:starve] conj (:m2 (:starved mp))))
                      res))]
      ;; Starvation
      (let [starve (:starve results)]
        {:starvation {:min (apply min starve)
                      :max (apply max starve)
                      :mean (mean starve)
                      :variance (variance starve)
                      :values starve}}))))


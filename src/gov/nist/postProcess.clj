;;; Various functions for studying output.

(ns gov.nist.MJPdes)

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

(defn analyze-results [filename]
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
                      :variance (variance starve)}}))))
      
      



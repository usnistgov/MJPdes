(ns gov.nist.MJPdes.util.utils-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer (cl-format pprint pp)]
            [clojure.spec.alpha :as s]
            [gov.nist.MJPdes.util.utils :as util]))


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


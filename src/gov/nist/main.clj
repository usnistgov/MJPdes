(ns gov.nist.main
  (:require [gov.nist.MJPdes :refer (main-loop map->Model map->Buffer map->ExpoMachine map->JobType)]
            [clojure.java.io :refer (as-file reader writer) :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
  (let [in-file  (if (= (nth args 0) "-i") (nth args 1) (nth args 3))
        out-file (if (= (nth args 0) "-o") (nth args 1) (nth args 3))]
    (if (.exists (io/as-file in-file))
      (binding [*ns* (find-ns 'gov.nist.MJPdes)]
        (let [in (java.io.PushbackReader. (io/reader in-file))
              model (eval (edn/read {:eof :eof} in))]
          (with-open [out-stream (io/writer out-file :encoding "UTF-8")]
            (main-loop model :out-stream out-stream))))
      (println "File" in-file "does not exist."))))

(ns gov.nist.main
  (:require [gov.nist.desim :refer (main-loop map->Model map->Buffer map->ExpoMachine map->JobType +diag+)]
            [clojure.pprint :refer (pprint)]
            [clojure.java.io :refer (as-file)]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
  (let [fname (first args)]
    (if (.exists (as-file fname))
      (binding [*ns* (find-ns 'gov.nist.desim)]
        (let [in (java.io.PushbackReader. (clojure.java.io/reader fname))
              model (eval (edn/read {:eof :eof} in))]
          (reset! +diag+ model)
          (println "Working...")
          (pprint (main-loop model))))
      (println "File" fname "does not exist."))))

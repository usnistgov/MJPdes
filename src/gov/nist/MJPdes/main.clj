(ns gov.nist.MJPdes.main
  (:require [gov.nist.MJPdes.core :refer (main-loop map->Model map->Buffer map->ExpoMachine map->JobType)]
            [clojure.java.io :refer (as-file reader writer) :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
  (let [arg-map (apply hash-map args)
        in-file  (get arg-map "-i")
        out-file (get arg-map "-o")]
    (if (.exists (io/as-file in-file))
      (binding [*ns* (find-ns 'gov.nist.MJPdes.core)]
        (let [in (java.io.PushbackReader. (io/reader in-file))
              model (eval (edn/read {:eof :eof} in))]
          (with-open [out-stream (if out-file (io/writer out-file :encoding "UTF-8") *out*)]
            (main-loop model :out-stream out-stream))))
      (println "File" in-file "does not exist."))))

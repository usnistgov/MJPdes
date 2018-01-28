(defproject gov.nist/MJPdes "0.3.0"
  :description "A discrete event simulation engine for Multi-Job Production"
  :url "https://github.com/usnistgov/MJPdes"
  :license {:name "See the github site for details"
            :url "https://github.com/usnistgov/MJPdes"}
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-binplus "0.6.4"]
            [lein-cljfmt "0.5.3"]]
  :dependencies [[org.clojure/clojure        "1.9.0"]
                 [incanter/incanter-core     "1.5.7"]]
  :bin {:name "MJPdes"
        #_:bin-path #_"~/bin"
        :bootclasspath false #_true
        :jvm-opts ["-server" "-Dfile.encoding=utf-8" "$JVM_OPTS" ]}
  :main gov.nist.MJPdes.main)

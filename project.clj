(defproject gov.nist/MJPdes "0.2.0"
  :description "A discrete event simulation engine for Multi-Job Production"
  :url "https://github.com/usnistgov/desim"
  :license {:name "See the github site for details"
            :url "https://github.com/usnistgov/MJPdes"}
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-bin "0.3.4"]
            [lein-cljfmt "0.5.3"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [medley "0.8.3"]
                 [incanter/incanter-core "1.9.1"]]
  :bin {:name "MJPdes"
        :bootclasspath true}
  :main gov.nist.main)

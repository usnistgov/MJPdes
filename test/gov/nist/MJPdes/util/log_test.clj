(ns gov.nist.MJPdes.util.log-test
  "Tests for MJPdes/util/log.clj"
  {:author "Peter Denno"}
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [gov.nist.MJPdes.core :refer (preprocess-model map->Model map->ExpoMachine map->Buffer map->JobType)]
            [gov.nist.MJPdes.util.utils :as util :refer (ppp ppprint)]
            [gov.nist.MJPdes.util.log :as log]))
                        
;;; POD If you recompile nn.clj after evaluating this, it won't happen. 
(stest/instrument) ; Instrument everything.

(def test-model
  (preprocess-model
   (map->Model
    {:line 
     {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0}) 
      :b1 (map->Buffer {:N 3})
      :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0})}
     :number-of-simulations 1
     :report {:log? true :max-lines 1000}
     :topology [:m1 :b1 :m2]
     :entry-point :m1
     :params {:warm-up-time 2000 :run-to-time 10000}
     :jobmix {:jobType1 (map->JobType {:portion 1.0 :w {:m1 1.0, :m2 1.1}})}})))

(deftest pretty-printing-messages
  (testing "that the form printed is correct."
    (is (= (log/pretty-buf
            test-model
            [{:act :ub, :m :m1, :clk 2133.0528055936406}
             {:act :aj, :j 623, :jt :jobType1, :ends 2134.0528055936406, :clk 2133.0528055936406}
             {:act :bl, :m :m1, :clk 2134.0528055936406}
             {:act :ej, :m :m2, :j 619, :ent 2120.652805593641, :clk 2136.1528055936406}
             {:act :sm, :bf :b1, :j 620, :n 3, :clk 2136.1528055936406}
             {:act :bj, :bf :b1, :j 623, :n 2, :clk 2136.1528055936406}
             {:act :ub, :m :m1, :clk 2136.1528055936406}
             {:act :aj, :j 624, :jt :jobType1, :ends 2137.313806869053, :clk 2136.1528055936406}
             {:act :bl, :m :m1, :clk 2137.313806869053}
             {:act :ej, :m :m2, :j 620, :ent 2123.752805593641, :clk 2139.2528055936405}
             {:act :sm, :bf :b1, :j 621, :n 3, :clk 2139.2528055936405}
             {:act :bj, :bf :b1, :j 624, :n 2, :clk 2139.2528055936405}
             {:act :ub, :m :m1, :clk 2139.2528055936405}
             {:act :aj, :j 625, :jt :jobType1, :ends 2140.6417982677162, :clk 2139.2528055936405}
             {:act :bl, :m :m1, :clk 2140.6417982677162}
             {:act :ej, :m :m2, :j 621, :ent 2126.852805593641, :clk 2142.3528055936404}
             {:act :sm, :bf :b1, :j 622, :n 3, :clk 2142.3528055936404}
             {:act :bj, :bf :b1, :j 625, :n 2, :clk 2142.3528055936404}
             {:act :ub, :m :m1, :clk 2142.3528055936404}])
           [{:clk 2136.1528, :act :m2-move-off, :m :m2, :ent 2120.6528, :mjpact :ej, :j 619}
            {:clk 2136.1528, :act :m2-start-job, :m :m2, :bf :b1, :n 3, :mjpact :sm, :j 620}
            {:clk 2139.2528, :act :m2-move-off, :m :m2, :ent 2123.7528, :mjpact :ej, :j 620}
            {:clk 2139.2528, :act :m2-start-job, :m :m2, :bf :b1, :n 3, :mjpact :sm, :j 621}
            {:clk 2142.3528, :act :m2-move-off, :m :m2, :ent 2126.8528, :mjpact :ej, :j 621}
            {:clk 2142.3528, :act :m2-start-job, :m :m2, :bf :b1, :n 3, :mjpact :sm, :j 622}
            {:clk 2133.0528, :act :m1-unblocked, :m :m1, :mjpact :ub}
            {:clk 2136.1528, :act :m1-unblocked, :m :m1, :mjpact :ub}
            {:clk 2139.2528, :act :m1-unblocked, :m :m1, :mjpact :ub}
            {:clk 2142.3528, :act :m1-unblocked, :m :m1, :mjpact :ub}
            {:clk 2133.0528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2134.0528, :mjpact :aj, :j 623}
            {:clk 2134.0528, :act :m1-blocked, :m :m1, :mjpact :bl}
            {:clk 2136.1528, :act :m1-move-off, :m :m1, :bf :b1, :n 2, :mjpact :bj, :j 623}
            {:clk 2136.1528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2137.3138, :mjpact :aj, :j 624}
            {:clk 2137.3138, :act :m1-blocked, :m :m1, :mjpact :bl}
            {:clk 2139.2528, :act :m1-move-off, :m :m1, :bf :b1, :n 2, :mjpact :bj, :j 624}
            {:clk 2139.2528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2140.6418, :mjpact :aj, :j 625}
            {:clk 2140.6418, :act :m1-blocked, :m :m1, :mjpact :bl}
            {:clk 2142.3528, :act :m1-move-off, :m :m1, :bf :b1, :n 2, :mjpact :bj, :j 625}]))))

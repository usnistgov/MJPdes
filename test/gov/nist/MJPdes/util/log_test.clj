(ns gov.nist.MJPdes.util.log-test
  "Tests for MJPdes/util/log.clj"
  {:author "Peter Denno"}
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.math.combinatorics :as comb]
            [gov.nist.MJPdes.core :as core]
            [gov.nist.MJPdes.util.utils :as util :refer (ppp ppprint)]
            [gov.nist.MJPdes.util.log :as log]))
                        
;;; POD If you recompile nn.clj after evaluating this, it won't happen. 
(stest/instrument) ; Instrument everything.

(defn =* [x y tol] (< (- x tol) y (+ x tol)))

(def test-model-bas
  (core/map->Model
   {:line 
    {:m1 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0 :discipline :BAS}) 
     :b1 (core/map->Buffer {:N 1})
     :m2 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0})}
    :number-of-simulations 1
    :report {:log? true :max-lines 1000 :up&down? true}
    :topology [:m1 :b1 :m2]
    :entry-point :m1
    :params {:warm-up-time 0 :run-to-time 100}
    :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 0.8, :m2 2.0}})}}))

(def test-model-bbs
  (core/preprocess-model
   (core/map->Model
    {:line 
     {:m1 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0 :discipline :BBS}) 
      :b1 (core/map->Buffer {:N 1})
      :m2 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0})}
     :number-of-simulations 1
     :report {:log? true :max-lines 1000 :up&down? true}
     :topology [:m1 :b1 :m2]
     :entry-point :m1
     :params {:warm-up-time 0 :run-to-time 100}
     :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 0.8, :m2 2.0}})}})))

(defn run-model-to-vec [test-model]
  (binding [*print-length* nil]
    (read-string
     (with-out-str
       (println "[")
       (core/main-loop test-model)
       (println "]")))))

;;; Once warmed-up, a reliable machine BAS follows this cycle:
;;; {:clk    5.6000 :act :m1-blocked        :m :m1 :mjpact :bl :line 5}
;;; {:clk    6.8000 :act :m2-complete-job   :m :m2 :mjpact :ej :ent 1.6 :j 3 :line 6}
;;; {:clk    6.8000 :act :m2-start-job      :m :m2 :mjpact :sm :bf :b1 :n 1 :j 4 :line 7}
;;; {:clk    6.8000 :act :m1-unblocked      :m :m1 :mjpact :ub :line 8}
;;; {:clk    6.8000 :act :m1-complete-job   :m :m1 :mjpact :bj :bf :b1 :n 0 :j 5 :line 9}
;;; {:clk    6.8000 :act :m1-start-job      :m :m1 :mjpact :aj :jt :jobType1 :ends 7.6 :j 6 :line 10}
(deftest reliable-BAS-pattern
  (testing "that BAS of reliable machines follows a fixed pattern with determined cycle time."
    (let [log (->> (run-model-to-vec test-model-bas)
                   (filter #(>= (:clk %) 5.6)) ; eliminate warm-up.
                   (partition 6))]
      (is (every? #(= [:m1-blocked :m2-complete-job :m2-start-job :m1-unblocked :m1-complete-job :m1-start-job]
                      (->> % (map :act) vec))
                  log))
      (is (every? #(=* 1.2 (- (-> % last :clk) (-> % first :clk)) 0.000000001) log)))))

;;; Once warmed-up, a reliable machine BBS follows this cycle:
;;; {:clk    4.8000 :act :m2-complete-job   :m :m2 :mjpact :ej :ent 0.8 :j 2 :line 14}
;;; {:clk    4.8000 :act :m2-start-job      :m :m2 :mjpact :sm :bf :b1 :n 1 :j 3 :line 15}
;;; {:clk    4.8000 :act :m1-unblocked      :m :m1 :mjpact :ub :line 16}
;;; {:clk    4.8000 :act :m1-start-job      :m :m1 :mjpact :aj :jt :jobType1 :ends 5.6 :j 4 :line 17}
;;; {:clk    5.6000 :act :m1-complete-job   :m :m1 :mjpact :bj :bf :b1 :n 0 :j 4 :line 18}
;;; {:clk    5.6000 :act :m1-blocked        :m :m1 :mjpact :bl :line 19}
(deftest reliable-BBS-pattern
  (testing "that BBS of reliable machines follows a fixed pattern with determined cycle time."
    (let [log (->> (run-model-to-vec test-model-bbs)
                   (filter #(>= (:clk %) 4.8)) ; eliminate warm-up.
                   (partition 6))]
      (is (every? #(= [:m2-complete-job :m2-start-job :m1-unblocked :m1-start-job :m1-complete-job :m1-blocked]
                      (->> % (map :act) vec))
                  log))
      (is (every? #(=* 0.8 (- (-> % last :clk) (-> % first :clk)) 0.000000001) log)))))

;;;===========================================================================
;;; Tests against known complete models
;;; NYI

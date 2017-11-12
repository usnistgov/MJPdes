(ns gov.nist.MJPdes.util.log-test
  "Tests for MJPdes/util/log.clj"
  {:author "Peter Denno"}
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.math.combinatorics :as comb]
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

(defn mixed-messages
  "Oh, that's too cute!"
  [msgs]
  (reduce (fn [all order]
            (conj all
                  (mapv #(nth msgs %) order)))
          []
          (comb/permutations (range (count msgs)))))

(defn test-model-sort-msgs
  "Exercise log/sort-messages on test-model"
  [msgs]
  (log/sort-messages test-model msgs))
  
(def unstarve-data
  [{:clk 2053.7211 :act :m1-move-off  :m :m1 :mjpact :bj :bf :b1, :n 0 :j 1724, :line 186}
   {:clk 2053.7211 :act :m1-start-job :m :m1 :mjpact :aj :jt :jobType1 :ends 2054.7211 :j 1725 :line 187}
   {:clk 2053.7211 :act :m2-unstarved :m :m2 :mjpact :us :line 184}
   {:clk 2053.7211 :act :m2-start-job :m :m2 :mjpact :sm :bf :b1 :n 1 :j 1724 :line 185}])

(def unblock-data
  [{:clk 2093.5011 :act :m2-move-off  :m :m2 :mjpact :ej :ent 2088.3425 :j 1757 :line 327}
   {:clk 2093.5011 :act :m2-start-job :m :m2 :mjpact :sm :bf :b1 :n 3 :j 1758 :line 328}
   {:clk 2093.5011 :act :m1-unblocked :m :m1 :mjpact :ub :line 329}
   {:clk 2093.5011 :act :m1-move-off  :m :m1 :mjpact :bj :bf :b1 :n 2 :j 1761 :line 330}
   {:clk 2093.5011 :act :m1-start-job :m :m1 :mjpact :aj :jt :jobType1 :ends 2094.9808 :j 1762 :line 331}])

(def block-data
  [{:clk 2093.4177 :act :m1-blocked :m :m1 :mjpact :bl :line 326}])

(def starve-data
  [{:clk 2053.4484 :act :m2-move-off :m :m2 :ent 2049.6847 :mjpact :ej :j 1723 :line 182}
   {:clk 2053.4484 :act :m2-starved :m :m2 :mjpact :st :line 183}])

(def ^:private failed (atom nil))

(defn failed-seqs
  "Set the 'problem' (the log we look at) to the m2-inhib-bas problem."
  [f]
  (reset! failed [])
  (f)) ; The canonical fixture function, in this case called using the 'once' procedure

(use-fixtures :once failed-seqs)

(defn every-order-ok? [correct-order]
  (let [result (remove (fn [order]
                         (= correct-order (test-model-sort-msgs order)))
                       (mixed-messages correct-order))]
    (if (empty? result)
      true
      (do (swap! failed #(into % result))
          false))))

(deftest unstarve-msg-ordering
  (testing "that it sorts messages correctly around unstarve."
    (is (every-order-ok? unstarve-data))))

(deftest unblock-msg-ordering
  (testing "that it sorts messages correctly around unblock."
    (is (every-order-ok? unblock-data))))

(deftest block-msg-ordering
  (testing "that it sorts messages correctly around blocking. So far, blocking happens by itself."
    (is (every-order-ok? block-data))))

(deftest starve-msg-ordering
  (testing "that it sorts messages correctly around starving."
    (is (every-order-ok? starve-data))))

(def right-order           
  [{:clk 2133.0528, :act :m1-unblocked, :m :m1, :mjpact :ub}
   {:clk 2133.0528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2134.0528, :mjpact :aj, :j 623}
   
   {:clk 2134.0528, :act :m1-blocked, :m :m1, :mjpact :bl}
   
   {:clk 2136.1528, :act :m2-move-off,  :m :m2, :ent 2120.6528, :mjpact :ej, :j 619}
   {:clk 2136.1528, :act :m2-start-job, :m :m2, :bf :b1, :n 3,  :mjpact :sm, :j 620}
   {:clk 2136.1528, :act :m1-unblocked, :m :m1, :mjpact :ub}
   {:clk 2136.1528, :act :m1-move-off,  :m :m1, :bf :b1, :n 2, :mjpact :bj, :j 623}
   {:clk 2136.1528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2137.3138, :mjpact :aj, :j 624}
   
   {:clk 2137.3138, :act :m1-blocked,   :m :m1, :mjpact :bl}
   
   {:clk 2139.2528, :act :m2-move-off,  :m :m2, :ent 2123.7528, :mjpact :ej, :j 620}
   {:clk 2139.2528, :act :m2-start-job, :m :m2, :bf :b1, :n 3,  :mjpact :sm, :j 621}
   {:clk 2139.2528, :act :m1-unblocked, :m :m1, :mjpact :ub}
   {:clk 2139.2528, :act :m1-move-off,  :m :m1, :bf :b1, :n 2, :mjpact :bj, :j 624}
   {:clk 2139.2528, :act :m1-start-job, :jt :jobType1, :m :m1, :ends 2140.6418, :mjpact :aj, :j 625}
            
   {:clk 2140.6418, :act :m1-blocked,   :m :m1, :mjpact :bl}
   
   {:clk 2142.3528, :act :m2-move-off,  :m :m2, :ent 2126.8528, :mjpact :ej, :j 621}
   {:clk 2142.3528, :act :m2-start-job, :m :m2, :bf :b1, :n 3,  :mjpact :sm, :j 622}
   {:clk 2142.3528, :act :m1-unblocked, :m :m1, :mjpact :ub}
   {:clk 2142.3528, :act :m1-move-off,  :m :m1, :bf :b1, :n 2,  :mjpact :bj, :j 625}])

(defn pick-from-atom!
  "Randomly remove one element from the atom and return it."
  [atom]
  (let [picked (nth @atom (rand-int (count @atom)))]
    (swap! atom (fn [a] (remove #(= picked %) a)))
    picked))

(defn right-order-random
  "Return a random order of all the messages from right-order."
  []
  (let [size (count right-order)
        patom (atom (range size))
        order (repeatedly size #(pick-from-atom! patom))]
    (mapv #(nth right-order %) order)))

(deftest pretty-printing-messages
  (testing "that the form printed is correct (more extensive message ordering too)."
    (is (= right-order (log/sort-messages test-model (right-order-random)))) ; wow am I lazy!
    (is (= right-order (log/sort-messages test-model (right-order-random))))
    (is (= right-order (log/sort-messages test-model (right-order-random))))
    (is (= right-order (log/sort-messages test-model (right-order-random))))
    (is (= right-order (log/sort-messages test-model (right-order-random))))))

  




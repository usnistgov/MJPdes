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

;;; ===== Small-size tests =============================================
(def test-model-bbs
  (core/preprocess-model
   (core/map->Model
    {:line 
     {:m1 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0 :discipline :BBS}) 
      :b1 (core/map->Buffer {:N 1})
      :m2 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0})}
     :number-of-simulations 1
     :report {:log? true :max-lines 27 :up&down? true}
     :topology [:m1 :b1 :m2]
     :entry-point :m1
     :params {:warm-up-time 0 :run-to-time 10}
     :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 0.8, :m2 2.0}})}})))

(def test-model-bas
  (core/preprocess-model
   (core/map->Model
    {:line 
     {:m1 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0 :discipline :BAS}) 
      :b1 (core/map->Buffer {:N 1})
      :m2 (core/map->ExpoMachine {:lambda 0.0 :mu 0.9 :W 1.0})}
     :number-of-simulations 1
     :report {:log? true :max-lines 27 :up&down? true}
     :topology [:m1 :b1 :m2]
     :entry-point :m1
     :params {:warm-up-time 0 :run-to-time 10}
     :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 0.8, :m2 2.0}})}})))

(deftest bbs-unblock-before-starting
  (testing "In BBS unblock before starting a new job." 
    (is (= true  (log/sort-two-messages
                  test-model-bbs      
                  {:clk    2.8000 :m :m1 :act :ub}
                  {:clk    2.8000 :m :m1 :act :aj})))
    (is (= true  (log/sort-two-messages
                  test-model-bbs      
                  {:clk    2.8000 :m :m1 :act :ub}
                  {:clk    2.8000 :m :m1 :act :bj})))
    (is (= false (log/sort-two-messages
                  test-model-bbs
                  {:clk    2.8000 :m :m1 :act :aj}
                  {:clk    2.8000 :m :m1 :act :ub})))
    (is (= false (log/sort-two-messages
                  test-model-bbs
                  {:clk    2.8000 :m :m1 :act :bj}
                  {:clk    2.8000 :m :m1 :act :ub})))
    ;; start job downstream before unblocking. 
    (is (= true  (log/sort-two-messages
                  test-model-bbs      
                  {:clk    2.8000 :m :m2 :act :sm}
                  {:clk    2.8000 :m :m1 :act :ub})))
    (is (= false (log/sort-two-messages
                  test-model-bbs
                  {:clk    2.8000 :m :m1 :act :ub}
                  {:clk    2.8000 :m :m2 :act :sm})))))

;;; Last machine does not block, so just 2 here
(deftest bas-block-after-done 
  (testing "In BAS block before done."
    (and (= true  (log/sort-two-messages
                   test-model-bas      
                   {:clk    2.8000 :m :m1 :act :bl}
                   {:clk    2.8000 :m :m1 :act :bj}))
         (= false (log/sort-two-messages
                   test-model-bas
                   {:clk    2.8000 :m :m1 :act :bj}
                   {:clk    2.8000 :m :m1 :act :bl})))))

;;; ===== Medium-size tests =============================================
(defn mixed-messages
  "Oh, that's too cute!"
  [msgs]
  (let [mixed (reduce (fn [all order]
                        (conj all
                              (mapv #(nth msgs %) order)))
                      []
                      (comb/permutations (range (count msgs))))]
    (println "mixed = " mixed)
    mixed))

;;; These only list ordering we care about. 
(def block-order-bas 
  [{:clk 2093.4177 :m :m1 :act :bl}
   {:clk 2093.4177 :m :m1 :act :bj}])

(def unblock-order-bas
  [{:clk 2093.5011 :m :m1 :act :ub}
   {:clk 2093.5011 :m :m1 :act :bj}
   {:clk 2093.5011 :m :m1 :act :aj}])

(def starve-order-bas
  [{:clk 2053.4484 :m :m2 :act :ej}
   {:clk 2053.4484 :m :m2 :act :st}])

(def unstarve-order-bas
  [{:clk 2053.7211 :m :m1 :act :bj}
   {:clk 2053.7211 :m :m1 :act :aj}
   {:clk 2053.7211 :m :m2 :act :us}
   {:clk 2053.7211 :m :m2 :act :sm}])

;;; ==== BBS -------------------
(def block-order-bbs
  [{:clk 2093.4177 :m :m1 :act :bj}
   {:clk 2093.4177 :m :m1 :act :bl}])

(def unblock-order-bbs
  [{:clk 2093.5011 :m :m2 :act :ej}
   {:clk 2093.5011 :m :m2 :act :sm}
   {:clk 2093.5011 :m :m1 :act :ub}
   {:clk 2093.5011 :m :m1 :act :bj}
   {:clk 2093.5011 :m :m1 :act :aj}])

;;; These are same as BAS. 
(def starve-order-bbs
  [{:clk 2053.4484 :m :m2 :act :ej}
   {:clk 2053.4484 :m :m2 :act :st}])

(def unstarve-order-bbs
  [{:clk 2053.7211 :m :m1 :act :bj}
   {:clk 2053.7211 :m :m1 :act :aj}
   {:clk 2053.7211 :m :m2 :act :us}
   {:clk 2053.7211 :m :m2 :act :sm}])

(def ^:private failed (ref nil))

(defn failed-seqs
  "The function for the test fixture. It is used so failures don't accumulate
   in failed across different runs of the tests."
  [f]
  (dosync (ref-set failed []))
  (f)) ; The canonical fixture function, in this case called using the 'once' procedure

(use-fixtures :once failed-seqs)

(defn every-order-ok? [model correct-order]
  (let [result (remove (fn [order]
                         (= correct-order (log/sort-messages model order)))
                       (mixed-messages correct-order))]
    (if (empty? result)
      true
      (dosync (alter failed #(into % result))
              false))))

(deftest block-msg-ordering-bas
  (testing "that it sorts messages correctly around blocking."
    (is (every-order-ok? test-model-bas block-order-bas))))

(deftest unblock-msg-ordering-bas
  (testing "that it sorts messages correctly around unblock."
    (is (every-order-ok? test-model-bas unblock-order-bas))))

(deftest starve-msg-ordering-bas
  (testing "that it sorts messages correctly around starving."
    (is (every-order-ok? test-model-bas starve-order-bas))))

(deftest unstarve-msg-ordering-bas
  (testing "that it sorts messages correctly around unstarve."
    (is (every-order-ok? test-model-bas unstarve-order-bas))))

;;; ------------ BBS -------------------------------------
(deftest block-msg-ordering-bbs
  (testing "that it sorts messages correctly around blocking."
    (is (every-order-ok? test-model-bbs block-order-bbs))))

(deftest unblock-msg-ordering-bbs
  (testing "that it sorts messages correctly around unblock."
    (is (every-order-ok? test-model-bbs unblock-order-bbs))))

(deftest starve-msg-ordering-bbs
  (testing "that it sorts messages correctly around starving."
    (is (every-order-ok? test-model-bbs starve-order-bbs))))

(deftest unstarve-msg-ordering-bbs
  (testing "that it sorts messages correctly around unstarve."
    (is (every-order-ok? test-model-bbs unstarve-order-bbs))))


;;; ===== Extensive ordering tests ========================================
(def bbs-input ; NB order here can be (is?) incorrect.
  [{:clk    0.0000 :m :m1 :act :aj :jt :jobType1 :ends 0.8 :j 1}
   {:clk    0.0000 :m :m2 :act :st}
   {:clk    0.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 1}
   {:clk    0.8000 :m :m1 :act :aj :jt :jobType1 :ends 1.6 :j 2}
   {:clk    0.8000 :m :m2 :act :us}
   {:clk    0.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 1}
   {:clk    1.6000 :m :m1 :act :bj :bf :b1 :n 0 :j 2}
   {:clk    1.6000 :m :m1 :act :bl}
   {:clk    2.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 2}
   {:clk    2.8000 :m :m1 :act :ub}
   {:clk    2.8000 :m :m1 :act :aj :jt :jobType1 :ends 3.6 :j 3}
   {:clk    2.8000 :m :m2 :act :ej :ent 0.0 :j 1}
   {:clk    3.6000 :m :m1 :act :bj :bf :b1 :n 0 :j 3}
   {:clk    3.6000 :m :m1 :act :bl}
   {:clk    4.8000 :m :m1 :act :aj :jt :jobType1 :ends 5.6 :j 4}
   {:clk    4.8000 :m :m2 :act :ej :ent 0.8 :j 2}
   {:clk    4.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 3}
   {:clk    4.8000 :m :m1 :act :ub}
   {:clk    5.6000 :m :m1 :act :bj :bf :b1 :n 0 :j 4}
   {:clk    5.6000 :m :m1 :act :bl}
   {:clk    6.8000 :m :m1 :act :aj :jt :jobType1 :ends 7.6 :j 5}
   {:clk    6.8000 :m :m2 :act :ej :ent 2.8 :j 3}
   {:clk    6.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 4}
   {:clk    6.8000 :m :m1 :act :ub}
   {:clk    7.6000 :m :m1 :act :bj :bf :b1 :n 0 :j 5}
   {:clk    7.6000 :m :m1 :act :bl}
   {:clk    8.8000 :m :m1 :act :aj :jt :jobType1 :ends 9.6 :j 6}])

(def test-bbs-out
  "Correct ordering (and form) of test output for block-before-service using bbs-input"
  [{:clk 0.0, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 0.8, :j 1, :line 1}
   {:clk 0.0, :act :m2-starved, :m :m2, :mjpact :st, :line 0}
   {:clk 0.8, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 1, :line 2}
   {:clk 0.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 1.6, :j 2, :line 3}
   {:clk 0.8, :act :m2-unstarved, :m :m2, :mjpact :us, :line 4}
   {:clk 0.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 1, :line 5}
   {:clk 1.6, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 2, :line 6}
   {:clk 1.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 7}
   {:clk 2.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 0.0, :j 1, :line 8}
   {:clk 2.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 2, :line 9}
   {:clk 2.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 10}
   {:clk 2.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 3.6, :j 3, :line 11}
   {:clk 3.6, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 3, :line 12}
   {:clk 3.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 13}
   {:clk 4.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 0.8, :j 2, :line 14}
   {:clk 4.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 3, :line 15}
   {:clk 4.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 16}
   {:clk 4.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 5.6, :j 4, :line 17}
   {:clk 5.6, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 4, :line 18}
   {:clk 5.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 19}
   {:clk 6.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 2.8, :j 3, :line 20}
   {:clk 6.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 4, :line 21}
   {:clk 6.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 22}
   {:clk 6.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 7.6, :j 5, :line 23}
   {:clk 7.6, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 5, :line 24}
   {:clk 7.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 25}])

(def bas-input ; NB order here can be (is?) incorrect.
  [{:clk    0.0000 :m :m1 :act :aj :jt :jobType1 :ends 0.8 :j 1}
   {:clk    0.0000 :m :m2 :act :st}
   {:clk    0.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 1}
   {:clk    0.8000 :m :m1 :act :aj :jt :jobType1 :ends 1.6 :j 2}
   {:clk    0.8000 :m :m2 :act :us}
   {:clk    0.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 1}
   {:clk    1.6000 :m :m1 :act :bj :bf :b1 :n 0 :j 2}
   {:clk    1.6000 :m :m1 :act :aj :jt :jobType1 :ends 2.4 :j 3}
   {:clk    2.4000 :m :m1 :act :bl}
   {:clk    2.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 3}
   {:clk    2.8000 :m :m1 :act :aj :jt :jobType1 :ends 3.6 :j 4}
   {:clk    2.8000 :m :m2 :act :ej :ent 0.0 :j 1}
   {:clk    2.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 2}
   {:clk    2.8000 :m :m1 :act :ub}
   {:clk    3.6000 :m :m1 :act :bl}
   {:clk    4.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 4}
   {:clk    4.8000 :m :m1 :act :aj :jt :jobType1 :ends 5.6 :j 5}
   {:clk    4.8000 :m :m2 :act :ej :ent 0.8 :j 2}
   {:clk    4.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 3}
   {:clk    4.8000 :m :m1 :act :ub}
   {:clk    5.6000 :m :m1 :act :bl}
   {:clk    6.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 5}
   {:clk    6.8000 :m :m1 :act :aj :jt :jobType1 :ends 7.6 :j 6}
   {:clk    6.8000 :m :m2 :act :ej :ent 1.6 :j 3}
   {:clk    6.8000 :m :m2 :act :sm :bf :b1 :n 1 :j 4}
   {:clk    6.8000 :m :m1 :act :ub}
   {:clk    7.6000 :m :m1 :act :bl}
   {:clk    8.8000 :m :m1 :act :bj :bf :b1 :n 0 :j 6}])

(def test-bas-out
  "Correct ordering (and form) of test output for block-after-service using bas-input."
  [{:clk 0.0, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 0.8, :j 1, :line 0}
   {:clk 0.0, :act :m2-starved, :m :m2, :mjpact :st, :line 1}
   {:clk 0.8, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 1, :line 2}
   {:clk 0.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 1.6, :j 2, :line 3}
   {:clk 0.8, :act :m2-unstarved, :m :m2, :mjpact :us, :line 4}
   {:clk 0.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 1, :line 5}
   {:clk 1.6, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 2, :line 6}
   {:clk 1.6, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 2.4, :j 3, :line 7}
   {:clk 2.4, :act :m1-blocked, :m :m1, :mjpact :bl, :line 8}
   {:clk 2.8, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 3, :line 9}
   {:clk 2.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 3.6, :j 4, :line 10}
   {:clk 2.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 0.0, :j 1, :line 11}
   {:clk 2.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 2, :line 12}
   {:clk 2.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 13}
   {:clk 3.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 14}
   {:clk 4.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 0.8, :j 2, :line 15}
   {:clk 4.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 3, :line 16}
   {:clk 4.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 17}
   {:clk 4.8, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 4, :line 18}
   {:clk 4.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 5.6, :j 5, :line 19}
   {:clk 5.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 20}
   {:clk 6.8, :act :m2-complete-job, :m :m2, :mjpact :ej, :ent 1.6, :j 3, :line 21}
   {:clk 6.8, :act :m2-start-job, :m :m2, :mjpact :sm, :bf :b1, :n 1, :j 4, :line 22}
   {:clk 6.8, :act :m1-unblocked, :m :m1, :mjpact :ub, :line 23}
   {:clk 6.8, :act :m1-complete-job, :m :m1, :mjpact :bj, :bf :b1, :n 0, :j 5, :line 24}
   {:clk 6.8, :act :m1-start-job, :m :m1, :mjpact :aj, :jt :jobType1, :ends 7.6, :j 6, :line 25}
   {:clk 7.6, :act :m1-blocked, :m :m1, :mjpact :bl, :line 26}])
   
(defn pick-from-ref!
  "Randomly remove one element from the ref and return it."
  [pref]
  (let [picked (rand-nth @pref)]
    (dosync (alter pref (fn [a] (remove #(= picked %) a))))
    picked))

(defn right-order-random
  "Return a random order of all the messages from right-order."
  [right-order]
  (let [size (count right-order)
        pref (ref (range size))
        order (repeatedly size #(pick-from-ref! pref))]
    (mapv #(nth right-order %) order)))

(defn randomize-top 
  "Return the model with the earliest messages in the :msg-buffer in random order"
  [model]
  (let [buf (:log-buf model)
        time (-> buf first :clk)
        top-buf (filterv #(== (:clk %) time) buf)
        bot-buf (filterv #(> (:clk %) time) buf)]
    (assoc model :log-buf (into (right-order-random top-buf) bot-buf))))
  
(defn random-push-log [test-model test-log]
  (let [model (-> test-model
                  (assoc :log-buf test-log))]
    (binding [log/*log-steady* (ref (log/steady-form model))]
      (read-string 
       (with-out-str
         (println "[")
         (loop [model model]
           (let [model (randomize-top model)]
             (if (== 8.8 (-> model :log-buf first :clk)) ; POD 8.8, fix me. 
               :done
               (recur (log/push-log model)))))
         (println "]"))))))

(deftest pprint-and-ordering
  (testing "unfortunately, two things simultaneously: pprint and sorting of 
   messages into logical order of occurrence."
    (is (= (random-push-log test-model-bbs bbs-input) test-bbs-out))
    (is (= (random-push-log test-model-bbs bbs-input) test-bbs-out))
    (is (= (random-push-log test-model-bbs bbs-input) test-bbs-out))
    (is (= (random-push-log test-model-bbs bbs-input) test-bbs-out))

    (is (= (random-push-log test-model-bas bas-input) test-bas-out))
    (is (= (random-push-log test-model-bas bas-input) test-bas-out))
    (is (= (random-push-log test-model-bas bas-input) test-bas-out))
    (is (= (random-push-log test-model-bas bas-input) test-bas-out))))

;;;===========================================================================
;;; Tests against known complete models

;;; NYI

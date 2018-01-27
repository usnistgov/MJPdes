(ns gov.nist.MJPdes.core-test
  "Tests for MJPdes/core.clj"
  {:author "Peter Denno"}
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [gov.nist.MJPdes.util.log :as log]
            [gov.nist.MJPdes.core :as core]
            [gov.nist.MJPdes.util.utils :as util]
            [incanter.stats :as s]
            [clojure.edn :as edn]
            [clojure.pprint :refer (cl-format pprint)]))

;;; POD If you recompile core.clj after evaluating this, it won't happen. 
(stest/instrument) ; Instrument everything

(def ^:private diag (atom nil))

;;; Do this in clojupyter!
#_(defn check-exponential-pdf
  []
  (into (sorted-map)
        (map (fn [[k v]] [k (count v)])
             (group-by #(Math/round (quot % 0.1))
                       (s/sample-exp 100000 :rate 0.9)))))

(defn =* [x y tol] (< (- x tol) y (+ x tol)))

(def job-ends-model
  {:clk 10.5
   :line {:m1 {:up&down [[0 :down 10.138318336024103]
                         [1 :up   10.991435470545209]
                         [2 :down 18.189385678401067]
                         [3 :up   18.507587845132132]
                         [4 :down 35.608712172391236]
                         [5 :up   36.852624249573]
                         [6 :down 63.65613563175827]
                         [7 :up   64.5679392394569]]}}})

(deftest job-ends-test
  (testing "that core/job-ends works.")
  (is (=* (core/end-time job-ends-model 15.0 :m1)
          26.309637637276275
          0.0000000000001)))

(def fdata 
  (seq [[0 :up    1.5669655022427902]
        [1 :down 10.138318336024103]
        [2 :up   10.991435470545209]
        [3 :down 18.189385678401067]
        [4 :up   18.507587845132132]
        [5 :down 35.608712172391236]
        [6 :up   36.852624249573]
        [7 :down 63.65613563175827]
        [8 :up   64.5679392394569]]))

;;; This is down-time before it ends. (Add this to 50 for job-updates!)
(+ (- 1.5669655022427902 1.0903780292458096)
   (- 10.991435470545209 10.138318336024103)
   (- 18.507587845132132 18.189385678401067)
   (- 36.852624249573    35.608712172391236))

(def fdata1
  (seq
   [[0 :down 10.138318336024103]
    [1 :up   10.991435470545209]
    [2 :down 18.189385678401067]
    [3 :up   18.507587845132132]
    [4 :down 35.608712172391236]
    [5 :up   36.852624249573]
    [6 :down 63.65613563175827]
    [7 :up   64.5679392394569]]))

(deftest job-tests
  (testing "whether job would end when expected given fixed up&down schedule"
    (let [tmodel (core/map->Model
                  {:line 
                   {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :up&down fdata :future [0 :down 1.0903780292458096]})} 
                   :topology [:m1]
                   :clock 0.0
                   :params {:current-job 0}
                   :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 50.0}})}})
          tjob (-> (core/new-job tmodel) (assoc :starts 0))
          dur  (util/job-requires tmodel tjob :m1)
          tmodel1 (core/map->Model
                   {:line 
                    {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :up&down fdata1 :future [0 :up 1.5669655022427902]})} 
                    :topology [:m1]
                    :clock 0.0
                    :params {:current-job 0}
                    :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 50.0}})}})]
      (is (== 50.0 dur))
      (is (=* 52.89181885143091   (core/end-time tmodel  dur :m1) 0.0000000001))
      (is (=* 53.982196880676725  (core/end-time tmodel1 dur :m1) 0.0000000001)))))

;(== 53.982196880676725 (+ 52.89181885143091 1.0903780292458096)

(defn test-bl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [m (-> (core/map->Model
               {:line {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                :topology [:m1]
                :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}
                :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 1.0}})}})
              (core/preprocess-model :check? false))]
    (binding [log/*log-steady* (atom (log/steady-form m))] ; create a log for computations.
      (-> m 
          (log/log {:act :bl, :m :m1, :clk 0.5})
          (log/log {:act :ub, :m :m1, :clk 0.5})
          (log/log {:act :bl, :m :m1, :clk 0.5})
          (log/log {:act :ub, :m :m1, :clk 0.5})
          (log/push-log 0.5) ; The above is realistic and should be cleaned by log/clean-log-buf.
          (log/log {:act :bl, :m :m1, :clk 1.0})
          (log/push-log 1.0)
          (log/log {:act :ub, :m :m1, :clk 2.0}) ; total block = 1
          (log/push-log 2.0)
          (log/log {:act :bl, :m :m1, :clk 3.0})
          (log/push-log 3.0)
          (log/log {:act :ub, :m :m1, :clk 4.0}) ; total block = 2
          (log/push-log 4.0)
          (log/push-log 5.0)                     ; block percent = 2/5
          (core/calc-basics @log/*log-steady*)))))

(defn test-sl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [model (-> (core/map->Model
                   {:line {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                    :topology [:m1]
                    :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}})
                  (core/preprocess-model :check? false))]
    (binding [log/*log-steady* (atom (log/steady-form model))] ; create a log for computations.
      (-> model
          (log/log {:act :st, :m :m1, :clk 0.5})
          (log/log {:act :us, :m :m1, :clk 0.5})
          (log/log {:act :st, :m :m1, :clk 0.5})
          (log/log {:act :us, :m :m1, :clk 0.5})
          (log/push-log 0.5) ; The above is realistic, but cleaned by log/clean-log-buf.
          (log/log {:act :st, :m :m1, :clk 1.0})
          (log/push-log 1.0)
          (log/log {:act :us, :m :m1, :clk 2.0})
          (log/push-log 2.0)
          (log/log {:act :st, :m :m1, :clk 3.0})
          (log/push-log 3.0)
          (log/log {:act :us, :m :m1, :clk 4.0})
          (log/push-log 4.0)
          (log/push-log 5.0)
          (core/calc-basics @log/*log-steady*)))))

(deftest log-testing-1
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-bl-log) :blocked :m1)))))

(deftest log-testing-2
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-sl-log) :starved :m1)))))

(defn test-efficiency
  "Run an exponential machine 1 million times. Calculate efficiency."
  [lambda mu]
  (let [start (core/expo-up&down-init-event lambda mu)
        up&down (core/expo-up&down lambda mu start)
        run-history (repeatedly 1000000 mc)
        final-time (last (last run-history))]  ; <===================== WORTH FINISHING!!!
    (/ 
     (loop [accum 0
            last-up 0
            hist run-history]
       (if hist
         (let [[state change-time] (first hist)]
           (if (= state :down)
             (recur (+ accum (- change-time last-up))
                    change-time
                    (next hist))
             (recur accum change-time (next hist))))
         accum))
     final-time)))

(deftest machine-test
  (testing "Expected exponential machine behavior"
    ;one-is-enough-for-now(is (< 0.49 (test-efficient 0.5 0.5) 0.51)) 
    (is (< 0.899 (test-efficiency 0.1 0.9) 0.901))))

(def diag-previous (atom nil))

(defn catch-up-machine-test []
  (let [result (atom true)]
    (dotimes [_ 10000]
      (reset! diag-previous 0)
      (let [m (as-> (core/map->ExpoMachine {:name :m1 :W 1.0 :up&down (core/exponential-up&down 0.1 0.9)}) ?m
                (assoc ?m :future ((:up&down ?m))))]
        (let [now (rand 3000.0)
              [_ time] (:future (core/catch-up-machine! m now))]
          (when (not (< @diag-previous now time))
            (println @diag-previous time now)
            (reset! result false)))))
    @result))

(deftest catch-up-machine
  (testing "Catch-up-machine works."
    (is (= true (catch-up-machine-test)))))

(def tf1
  (core/preprocess-model
   (core/map->Model
    {:line 
     {:m1 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) 
      :b1 (core/map->Buffer {:N 3})
      :m2 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :b2 (core/map->Buffer {:N 5})
      :m3 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
      :b3 (core/map->Buffer {:N 1})
      :m4 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
      :b4 (core/map->Buffer {:N 1})
      :m5 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
     :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
     :entry-point :m1 ; 20,000 200,000
     :params {:warm-up-time 2000 :run-to-time 20000}  
     :jobmix {:jobType1 (core/map->JobType {:portion 1.0
                                       :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})}})))

(def tp1 {:blocked
         {:m1 0.1860
          :m2 0.0346
          :m3 0.1046
          :m4 0.0398
          :m5 0.0},
         :starved
         {:m1 0.0
          :m2 0.0089
          :m3 0.0186
          :m4 0.0462
          :m5 0.1860}})

(def tp2 {:blocked
          {:m1 0.1501
          :m2 0.0001
          :m3 0.0258
          :m4 0.0061
          :m5 0.0},
         :starved
         {:m1 0.0
          :m2 0.0001
          :m3 0.0578
          :m4 0.0371
          :m5 0.1501}})

(deftest bneck-test
  (testing "Check bottleneck identification"
    (is (= (:bottleneck-machine (core/calc-bneck tp1 tf1)) :m4))
    (is (= (:bottleneck-machine (core/calc-bneck tp2 tf1)) :m2))))

;;; m1 |---- 2 ----|---- 2 ----|---- 2 ----|BBBBB|---- 2 ----|
;;; m2             |-------------- 5 ------------|                                        
(defn load-m1
  "No warm-up. M2 takes almost the whole simulation to process a part."
  []
  (core/main-loop (core/map->Model
              {:line ; POD if you go really crazy with lambda and mu it will hang. Not investigated!
               {:m1 (core/map->ExpoMachine {:lambda 0.0001 :mu 100.0 :W 1.0}) 
                :b1 (core/map->Buffer {:N 1})
                :m2 (core/map->ExpoMachine {:lambda 0.0001 :mu 100.0 :W 1.0})}
               :report {:log? true :max-lines 3000 :diag-log-buf? true}
               :number-of-simulations 1
               :topology [:m1 :b1 :m2]
               :entry-point :m1
               :params {:warm-up-time 0 :run-to-time 10}
               :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 2.0, :m2 5.0}})}})))

;;; Note I haven't finished this test, of course. :diag-log-buf? isn't working. 

;;; POD I think that there still may be a bug with 'confused' log data.
;;; See the text around new-blocked? I think there is a situation that could be
;;; tested with something like "load-m1" above where block/unblock messages will be 
;;; processed in the wrong order. 

#{{:act :aj, :j 1, :jt :jobType1, :ends 2.0, :clk 0.0}
  {:act :st, :m :m2, :clk 0.0}
  {:act :bj, :bf :b1, :j 1, :n 0, :clk 2.0}
  {:act :aj, :j 2, :jt :jobType1, :ends 4.0, :clk 2.0}
  {:act :sm, :bf :b1, :j 1, :n 1, :clk 2.0}
  {:act :us, :m :m2, :clk 2.0}
  {:act :bj, :bf :b1, :j 2, :n 0, :clk 4.0}
  {:act :aj, :j 3, :jt :jobType1, :ends 6.0, :clk 4.0}
  {:act :bl, :m :m1, :clk 6.0}
  {:act :ej, :m :m2, :j 1, :ent 0.0, :clk 7.0}
  {:act :sm, :bf :b1, :j 2, :n 1, :clk 7.0}
  {:act :bj, :bf :b1, :j 3, :n 0, :clk 7.0}
  {:act :ub, :m :m1, :clk 7.0}
  {:act :aj, :j 4, :jt :jobType1, :ends 9.0, :clk 7.0}
  {:act :bl, :m :m1, :clk 9.0}}

;;; Instrument works best when down here. 
(stest/instrument)

:loaded-it-all
  
#_(def f0
  (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0}) 
     :b1 (map->Buffer {:N 2})
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0})}
    :number-of-simulations 1
    :report {:log? true :max-lines 1000}
    :topology [:m1 :b1 :m2]
    :entry-point :m1
    :params {:warm-up-time 2000 :run-to-time 10000}
    :jobmix {:jobType1 (map->JobType {:portion 1.0 :w {:m1 2.0, :m2 0.8}})}}))

#_(defn runit []
  (with-open [w (clojure.java.io/writer "/tmp/f0.clj")]
    (core/main-loop f0 :out-stream w)))

#_(def f1
  (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}}) 
     :b1 (map->Buffer {:N 3})
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b2 (map->Buffer {:N 5})
     :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b3 (map->Buffer {:N 1})
     :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b4 (map->Buffer {:N 1})
     :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})}
    :number-of-simulations 10
    :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
    :entry-point :m1 ; 
    :params {:warm-up-time 2000 :run-to-time 20000}  
    :jobmix {:jobType1 (map->JobType {:portion 1.0
                                      :w {:m1 1.0,
                                          :m2 1.0,
                                          :m3 1.0,
                                          :m4 1.0,
                                          :m5 1.01}})}}))

#_(def f2
    (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) ; the definition of machine :m1
     :b1 (map->Buffer {:N 3})                             ; the definition of buffer :b1
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
     :b2 (map->Buffer {:N 5})
     :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
     :b3 (map->Buffer {:N 1})
     :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
     :b4 (map->Buffer {:N 1})
     :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
    :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5] ; the arrangement of the line
    :entry-point :m1 ; where jobs start
    :params {:warm-up-time 20000 :run-to-time 100000}  
    :jobmix {:jobType1 (map->JobType {:portion 0.8 ; 80% of jobs will be of type :jobType1.
                                      :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})
             :jobType2 (map->JobType {:portion 0.2 ; 20% of jobs will be of type :jobType2.
                                      :w {:m1 1.0, :m2 1.0, :m3 1.3, :m4 1.0, :m5 1.0}})}}))

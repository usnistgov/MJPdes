(ns gov.nist.MJPdes-test
  "Tests for MJPdes.clj"
  {:author "Peter Denno"}
  (:require [clojure.test :refer :all]
            [gov.nist.MJPdes :refer :all :as mjp]
            [incanter.stats :as s]
            [clojure.edn :as edn]
            [clojure.pprint :refer (cl-format pprint)]))

;;; This is out of date, maybe not hopelessly so but pretty close!

(def ^:private diag (atom nil))

#_(defn check-exponential-pdf
  []
  (into (sorted-map)
        (map (fn [[k v]] [k (count v)])
             (group-by #(Math/round (quot % 0.1))
                       (s/sample-exp 100000 :rate 0.9)))))

(defn fdata []
  (let [cnt (atom -1)]
    (fn []
      (nth 
       '([:up    1.5669655022427902]
         [:down 10.138318336024103]
         [:up   10.991435470545209]
         [:down 18.189385678401067]
         [:up   18.507587845132132]
         [:down 35.608712172391236]
         [:up   36.852624249573]
         [:down 63.65613563175827]
         [:up   64.5679392394569])
       (swap! cnt inc)))))

;;; This is down-time before it ends. (Add this to 50 for job-updates!)
(+ (- 1.5669655022427902 1.0903780292458096)
   (- 10.991435470545209 10.138318336024103)
   (- 18.507587845132132 18.189385678401067)
   (- 36.852624249573    35.608712172391236))

(defn fdata1
  []
  (let [cnt (atom -1)]
    (fn []
      (nth 
       '(
         [:down 10.138318336024103]
         [:up   10.991435470545209]
         [:down 18.189385678401067]
         [:up   18.507587845132132]
         [:down 35.608712172391236]
         [:up   36.852624249573]
         [:down 63.65613563175827]
         [:up   64.5679392394569])
       (swap! cnt inc)))))

(deftest job-tests
  (testing "whether job would end when expected given fixed up&down schedule"
    (let [tmodel (mjp/map->Model
                  {:line 
                   {:m1 (mjp/map->ExpoMachine {:name :m1 :W 1.0 :mchain (fdata) :future [:down 1.0903780292458096]})} 
                   :topology [:m1]
                   :clock 0.0
                   :params {:current-job 0}
                   :jobmix {:jobType1 (mjp/map->JobType {:portion 1.0 :w {:m1 50.0}})}})
          tjob (-> (mjp/new-job tmodel) (assoc :starts 0))]
      (is (== 50.0 (mjp/job-requires tmodel tjob (mjp/lookup tmodel :m1))))
      (is (= [52.89181885143091  [:down 63.65613563175827]]
             (mjp/job-updates! tmodel  tjob (mjp/lookup tmodel :m1))))

      (let [tmodel1 (mjp/map->Model
                     {:line 
                      {:m1 (mjp/map->ExpoMachine {:name :m1 :W 1.0 :mchain (fdata1) :future [:up 1.5669655022427902]})} 
                      :topology [:m1]
                      :clock 0.0
                      :params {:current-job 0}
                      :jobmix {:jobType1 (mjp/map->JobType {:portion 1.0 :w {:m1 50.0}})}})]
        (is (= [53.982196880676725 [:down 63.65613563175827]]
               (mjp/job-updates! tmodel1 tjob (mjp/lookup tmodel1 :m1))))))))

;(== 53.982196880676725 (+ 52.89181885143091 1.0903780292458096)

(defn test-bl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [model (-> (mjp/map->Model
                   {:line {:m1 (mjp/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                    :topology [:m1]
                    :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}})
                  mjp/preprocess-model)]
    (binding [mjp/*log-for-compute* (atom (log-form model))] ; create a log for computations.
      (-> model 
          (mjp/log {:act :bl, :m :m1, :clk 0.5})
          (mjp/log {:act :ub, :m :m1, :clk 0.5})
          (mjp/log {:act :bl, :m :m1, :clk 0.5})
          (mjp/log {:act :ub, :m :m1, :clk 0.5})
          (mjp/push-log 0.5) ; The above is realistic and should be cleaned by mjp/clean-log-buf.
          (mjp/log {:act :bl, :m :m1, :clk 1.0})
          (mjp/push-log 1.0)
          (mjp/log {:act :ub, :m :m1, :clk 2.0}) ; total block = 1
          (mjp/push-log 2.0)
          (mjp/log {:act :bl, :m :m1, :clk 3.0})
          (mjp/push-log 3.0)
          (mjp/log {:act :ub, :m :m1, :clk 4.0}) ; total block = 2
          (mjp/push-log 4.0)
          (mjp/push-log 5.0)                     ; block percent = 2/5
          (mjp/calc-basics @mjp/*log-for-compute*)))))

(defn test-sl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [model (-> (mjp/map->Model
                   {:line {:m1 (mjp/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                    :topology [:m1]
                    :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}})
                  mjp/preprocess-model)]
    (binding [mjp/*log-for-compute* (atom (log-form model))] ; create a log for computations.
      (-> model
          (mjp/log {:act :st, :m :m1, :clk 0.5})
          (mjp/log {:act :us, :m :m1, :clk 0.5})
          (mjp/log {:act :st, :m :m1, :clk 0.5})
          (mjp/log {:act :us, :m :m1, :clk 0.5})
          (mjp/push-log 0.5) ; The above is realistic, but cleaned by mjp/clean-log-buf.
          (mjp/log {:act :st, :m :m1, :clk 1.0})
          (mjp/push-log 1.0)
          (mjp/log {:act :us, :m :m1, :clk 2.0})
          (mjp/push-log 2.0)
          (mjp/log {:act :st, :m :m1, :clk 3.0})
          (mjp/push-log 3.0)
          (mjp/log {:act :us, :m :m1, :clk 4.0})
          (mjp/push-log 4.0)
          (mjp/push-log 5.0)
          (mjp/calc-basics @mjp/*log-for-compute*)))))

(deftest log-testing-1
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-bl-log) :blocked :m1)))))

(deftest log-testing-2
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-sl-log) :starved :m1)))))

(defn test-efficient
  "Run an exponential machine 1 million times. Calculate efficiency."
  [lambda mu]
  (let [mc (mjp/exponential-up&down lambda mu)
        run-history (repeatedly 1000000 mc)
        final-time (last (last run-history))]
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
    (is (< 0.899 (test-efficient 0.1 0.9) 0.901))))

(def diag-previous (atom nil))

(defn catch-up-machine-test []
  (let [result (atom true)]
    (dotimes [_ 10000]
      (reset! diag-previous 0)
      (let [m (as-> (mjp/map->ExpoMachine {:name :m1 :W 1.0 :mchain (mjp/exponential-up&down 0.1 0.9)}) ?m
                (assoc ?m :future ((:mchain ?m))))]
        (let [now (rand 3000.0)
              [_ time] (:future (mjp/catch-up-machine! m now))]
          (when (not (< @diag-previous now time))
            (println @diag-previous time now)
            (reset! result false)))))
    @result))

(deftest catch-up-machine
  (testing "Catch-up-machine works."
    (is (= true (catch-up-machine-test)))))

(def tf1
  (mjp/preprocess-model
   (mjp/map->Model
    {:line 
     {:m1 (mjp/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) 
      :b1 (mjp/map->Buffer {:N 3})
      :m2 (mjp/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :b2 (mjp/map->Buffer {:N 5})
      :m3 (mjp/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
      :b3 (mjp/map->Buffer {:N 1})
      :m4 (mjp/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
      :b4 (mjp/map->Buffer {:N 1})
      :m5 (mjp/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
     :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
     :entry-point :m1 ; 20,000 200,000
     :params {:warm-up-time 2000 :run-to-time 20000}  
     :jobmix {:jobType1 (mjp/map->JobType {:portion 1.0
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
    (is (= (:bottleneck-machine (mjp/calc-bneck tp1 tf1)) :m4))
    (is (= (:bottleneck-machine (mjp/calc-bneck tp2 tf1)) :m2))))


        

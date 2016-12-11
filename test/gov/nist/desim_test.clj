(ns gov.nist.desim-test
  (:require [clojure.test :refer :all]
            [gov.nist.desim :refer :all :as d]
            [incanter.stats :as s]
            [clojure.edn :as edn]
            [clojure.pprint :refer (cl-format pprint)]))

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

;;; This is down-time before it ends. (Add to 50 for job-updates!)
(+ (- 1.5669655022427902 1.0903780292458096)
   (- 10.991435470545209 10.138318336024103)
   (- 18.507587845132132 18.189385678401067)
   (- 36.852624249573    35.608712172391236))

(def tmodel
      (map->Model
       {:line 
        {:m1 (map->ExpoMachine {:name :m1 :W 1.0 :mchain (fdata) :future [:down 1.0903780292458096]})} 
        :topology [:m1]
        :clock 0.0
        :params {:current-job 0}
        :jobmix {:jobType1 (map->JobType {:portion 1.0 :w {:m1 50.0}})}}))

(defn check-exponential-pdf
  []
  (into (sorted-map)
        (map (fn [[k v]] [k (count v)])
             (group-by #(Math/round (quot % 0.1))
                       (s/sample-exp 100000 :rate 0.9)))))

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

(def tmodel1
      (map->Model
       {:line 
        {:m1 (map->ExpoMachine {:name :m1 :W 1.0 :mchain (fdata1) :future [:up 1.5669655022427902]})} 
        :topology [:m1]
        :clock 0
        :params {:current-job 0}
        :jobmix {:jobType1 (map->JobType {:portion 1.0 :w {:m1 50.0}})}}))

(def tjob (-> (d/new-job tmodel) (assoc :starts 0)))

(deftest job-tests
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (= 50.0 (d/job-requires tmodel tjob (d/lookup tmodel :m1))))
    (is (= [52.89181885143091 [:down 63.65613563175827]]
           (d/job-updates! tmodel tjob (d/lookup tmodel :m1))))
    (is (= [53.982196880676725 [:down 63.65613563175827]]
           (d/job-updates! tmodel1 tjob (d/lookup tmodel1 :m1))))))

(+ 52.89181885143091 1.0903780292458096)

(defn t-analyze-results [model]
  "Most of the analyze-results code. Yeah, of course I know!...."
  (let [in (java.io.PushbackReader. (clojure.java.io/reader @+out-filename+))
        edn-seq (repeatedly (partial edn/read {:eof :eof :readers {'function (fn [x])}} in))
        res (atom nil)
        warm-up-time (:warm-up-time (:params model))
        run-time (- (:run-to-time (:params model)) warm-up-time)]
      (dorun
       (reset!
        res
        (reduce 
         (fn [r o]
           (if (and (:act o) (> (:clk o) warm-up-time))
             (case (:act o)
               :bj (buf+ r o)
               :sm (buf- r o)
               :ej (end-job r o)
               :bl (block+ r o) 
               :ub (block- r o)
               :st (starve+ r o)
               :us (starve- r o))
             r))
         (results-form model)
         (take-while #(not= :eof %) edn-seq))))
      (as-> @res ?r
        (calc-basics ?r model))))


(defn test-bl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (reset! d/+out-filename+ (cl-format nil "resources/output/w-~A.clj" (now)))
  (with-open [w (clojure.java.io/writer @d/+out-filename+)]
    (binding [log (d/print-logger w)]
      (log {:act :bl, :m :m1, :clk 1.0})
      (log {:act :ub, :m :m1, :clk 1.0})
      (log {:act :bl, :m :m1, :clk 1.0})
      (log {:act :ub, :m :m1, :clk 1.0})
      (log {:act :bl, :m :m1, :clk 1.0}) ; the above is realistic!
      (log {:act :ub, :m :m1, :clk 2.0})
      (log {:act :bl, :m :m1, :clk 3.0})
      (log {:act :ub, :m :m1, :clk 4.0})
      (let [model (map->Model
                   {:line {:m1 (map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                   :topology [:m1]
                   :clock 0.0
                   :params {:current-job 0}})]
        (-> model
            (preprocess-model)
            (assoc-in [:params :warm-up-time] 0.0)
            (assoc-in [:params :run-to-time] 5.0)
            (t-analyze-results))))))

(defn test-sl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (reset! d/+out-filename+ (cl-format nil "resources/output/w-~A.clj" (now)))
  (with-open [w (clojure.java.io/writer @d/+out-filename+)]
    (binding [log (d/print-logger w)]
      (log {:act :st, :m :m1, :clk 1.0})
      (log {:act :us, :m :m1, :clk 1.0})
      (log {:act :st, :m :m1, :clk 1.0})
      (log {:act :us, :m :m1, :clk 1.0})
      (log {:act :st, :m :m1, :clk 1.0}) ; the above is realistic!
      (log {:act :us, :m :m1, :clk 2.0})
      (log {:act :st, :m :m1, :clk 3.0})
      (log {:act :us, :m :m1, :clk 4.0})
      (let [model (map->Model
                   {:line {:m1 (map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                   :topology [:m1]
                   :clock 0.0
                   :params {:current-job 0}})]
        (-> model
            (preprocess-model)
            (assoc-in [:params :warm-up-time] 0.0)
            (assoc-in [:params :run-to-time] 5.0)
            (t-analyze-results))))))

(deftest log-testing-1
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (= 0.4 (:m1 (:blocked (test-bl-log)))))))

(deftest log-testing-2
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (= 0.4 (:m1 (:starved (test-sl-log)))))))


(defn test-efficient
  "Run an exponential machine 1 million times. Calculate efficiency."
  [lambda mu]
  (let [mc (d/exponential-up&down lambda mu)
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

#_(defn catch-up-machine []
  (let [result (atom true)]
    (dotimes [n 10000]
      (reset! d/+diag-previous+ 0)
      (let [m (as-> (map->ExpoMachine {:name :m1 :W 1.0 :mchain (d/exponential-up&down 0.1 0.9)}) ?m
                (assoc ?m :future ((:mchain ?m))))]
        (let [now (rand 3000.0)
              [_ time] (:future (d/catch-up-machine! m now))]
          (when (not (< @d/+diag-previous+ now time))
            (println @d/+diag-previous+ time now)
            (reset! result false)))))
    @result))

#_(deftest catch-up-machine
  (testing "Catch-up-machine works (Requires uncommenting atom around d/catch-up-machine)."
    (is (= true (catch-up-machine)))))

(def tf1
  (d/preprocess-model
   (map->Model
    {:line 
     {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) 
      :b1 (map->Buffer {:N 3})
      :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :b2 (map->Buffer {:N 5})
      :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
      :b3 (map->Buffer {:N 1})
      :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
      :b4 (map->Buffer {:N 1})
      :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
     :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
     :entry-point :m1 ; 20,000 200,000
     :params {:warm-up-time 2000 :run-to-time 20000}  
     :jobmix {:jobType1 (map->JobType {:portion 1.0
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
    (is (= (:bneck (d/calc-bneck tp1 tf1)) :m4))
    (is (= (:bneck (d/calc-bneck tp2 tf1)) :m2))))


        

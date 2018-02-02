(ns gov.nist.MJPdes.core
  "Multi-job production (mixed-model production) discrete event simulation."
  {:author "Peter Denno"}
  (:require [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.pprint :refer (cl-format pprint)]
            [clojure.spec.test.alpha :as stest]
            [incanter.stats :as stats :refer (sample-exp)]
            [gov.nist.MJPdes.util.utils :as util :refer (ppprint ppp)]
            [gov.nist.MJPdes.util.log :as log]))

;;; Purpose: Implements a discrete event simulation engine for multi-job production.

;;; ToDo: Find a way around the pesky inconsistency that arises when the defrecords
;;;       here are recompiled; when that happens old models are seen as having 
;;;       incorrect types. (They don't satisfy instance?, for example.)

;;; Q: 
;;;   - BLOCKED means:
;;;        - There is a completed job that can't move to the buffer OR 
;;;        - The above plus the machine is not starved ?
;;; A: Doesn't matter much with these models, but if you had a machine that
;;;    had much greater capacity than the others, it would starve often.
;;;    I would not consider it blocked when it also going to be starved when
;;;    space in the buffer eventually is available. 
;;;   - fix blocked-requires-not-starved

;;; POD Library users aren't going to like this! Needed for debugging.
;;; Possibly wasn't needed. 
;;; (set! *print-length* 30) 
(def ^:private diag (atom nil))
(def ^:private +defaults+
  {:jobs-move-to-down-machines? false
   :time-format "~10,4f"})

(s/check-asserts true)

(defrecord Model [line entry-point jobmix params topology print])

(defrecord Machine [])
(defrecord ReliableMachine [name status])  ; For specific tests. Assume W=1
(defrecord ExpoMachine     [name lambda mu status W up&down])

(defn reliable? [m]
    (instance? ReliableMachine m))

(defn machine? [m]
  (or (instance? ExpoMachine m)
      (instance? ReliableMachine m)
      (instance? Machine m)))

(defn machines
  "Return the machine maps of the model line."
  [model]
  (let [line (:line model)]
    (map #(% line) (:machines model))))

(defrecord Buffer [N holding])
(defrecord InfiniteBuffer [])
(defrecord DedicatedBuffer [])

(defn buffer? [b]
  (or (instance? Buffer b)
      (instance? InfiniteBuffer b)
      (instance? DedicatedBuffer b)))

(defrecord JobType [w])

(defrecord Job [type
                id
                enters ; clock when entered on FIRST MACHINE
                starts ; clock at which it will start CURRENT MACHINE
                ends]) ; clock at which it will finish CURRENT MACHINE

(s/def ::ends float?)
(s/def ::starts float?)
(s/def ::enters float?)
(s/def ::id integer?)
(s/def ::type keyword?)
(s/def ::Job (s/keys :req-un [::type ::id ::enters ::starts ::ends]))

(s/def ::non-neg (s/and number? #(not (neg? %))))
(s/def ::pos     (s/and number? pos?))
(s/def ::posint  (s/and integer? pos?))

(s/def ::W ::pos)
(s/def ::mu ::pos)
(s/def ::lambda ::non-neg)
(s/def ::up&down seq?)
(s/def ::status (s/nilable ::Job))
(s/def ::ExpoMachine (s/keys :req-un [::lambda ::mu ::W ::status ::up&down]))

(s/def ::N ::posint)
(s/def ::Buffer (s/keys :req-un [::N]))

(s/def ::event (s/tuple [number? #{:up :down} number?]))

(s/def ::rmachine #(and (not (contains? % :lambda))
                        (not (contains? % :mu))
                        (not (contains? % :N))))

(s/def ::warm-up-time ::non-neg)
(s/def ::run-to-time ::pos)
(s/def ::params (s/keys ::warm-up-time ::run-to-time))

(s/def ::w (s/map-of keyword? ::pos))
(s/def ::portion ::pos)
(s/def ::JobType (s/keys :req-un [::portion ::w])) 

(s/def ::max-lines ::posint)
(s/def ::log boolean?)
(s/def ::report (s/keys :req-un [::log?] :req-opt [::max-lines]))

;(s/def ::number-of-simulations ::posint)
(s/def ::jobmix (s/and (s/map-of keyword? ::JobType) #(>= (count %) 1)))
(s/def ::machine (s/or :rmachine ::rmachine :emachine ::ExpoMachine))
(s/def ::equipment (s/or :machine ::machine :buffer ::Buffer))
(s/def ::line (s/and (s/map-of keyword? ::equipment) #(>= (count %) 3)))
(s/def ::Model (s/keys :req-un [::line ::topology ::entry-point ::jobmix ::report ::params]))

(s/fdef advance-clock 
        :args (s/and (s/cat :model ::Model :new-clock ::non-neg)
                     #(<= (-> (:model %) :clock) (:new-clock %)))
        :ret ::Model)

(defn advance-clock
  "Move clock and machines forward in time. This is the only 
   way in which the clock is advanced." 
  [model new-clock]
  (let [clock (:clock model)] ; POD the following superfluous when s/fdef. 
    (when (> clock new-clock) (throw (ex-info "clock running backwards"
                                              {:clock clock :new-clock new-clock})))
    (assoc model :clock new-clock)))

;;;  end-time = last time the machine starts up plus what remains to be achieved at that time.
(defn end-time
  "Scan through :up&down and determine when the job will end."
  [model dur m]
  (loop [not-achieved dur
         now (:clock model)
         up&down (drop-while #(< (nth % 2) now) (-> model :line m :up&down))]
      (let [[_ event etime] (first (take 1 up&down))]
        (if (= event :up) ; then machine was down; move on. 
          (recur not-achieved etime (drop 1 up&down))
          (if (> (- etime now) not-achieved) ; then there is enough here to get it done.
            (+ now not-achieved) ; that's end-time. 
            (recur (- not-achieved (- etime now)) ; otherwise add it in and continue
                   etime
                   (drop 1 up&down)))))))

;;; N.B. I don't in-line this in expo-up&down nor call it from there because
;;; I'd like to keep it possible to drop the head of the lazy-seq (BTW, that's
;;; also the reason for the index number (first element of event vector.) 
(defn expo-up&down-init-event
  "Return the 3-element vector event structure for first event of a machine."
  [lambda mu]
  (let [up? (< (rand) (/ mu (+ lambda mu)))]
    [0 (if up? :down :up) (stats/sample-exp 1 :rate (if up? lambda mu))]))

(defn p-state-change
  "Calculate probability of a flip = 1- e^(-rate t) : exponential CDF."
  [t rate]
  (- 1.0 (Math/pow Math/E (- (* rate t)))))

;;; Choose jump times T using sampling on exponential with lambda or mu.
;;; Use the probability of the p-up/down(T) to choose a new state (maybe same state)
;;; Save state for next call.
;;; This implements *time-dependent failure*, that is, breakdown and repair are
;;; insensitive to time blocked or starved. The alternative is called *operation-dependent
;;; failure* machine breakdowns cannot occur while blocked or starved. Semyon's book
;;; notes that in practice the difference might be small (1-3%).
(defn expo-up&down
  "Return a lazy-seq generator for the up/down events of an exponential machine.
   The events (and start arg) look like this: [<index number> (:up | :down) <time-point>]."
  [lambda mu start]
  (if (== lambda 0)
    '([0 :down ##Inf])
    (iterate (fn [[n event etime]]
             [(inc n)
              (if (= :up event) :down :up)
              (let [up-now? (= event :up)
                    rate (if up-now? lambda mu)
                    some-num (rand)
                    t-now etime]
                (loop [T-delta (stats/sample-exp 1 :rate rate)]
                  (if (< some-num (p-state-change T-delta rate))
                    (+ t-now T-delta)
                    (recur (+ T-delta
                              (stats/sample-exp 1 :rate rate))))))])
           start)))

(defn machine-future
  "Return the next event for the machine."
  [model m]
  (let [now (:clock model)]
     (first (take 1 (drop-while #(< (nth % 2) now) (-> model :line m :up&down))))))

;;; :aj - add-job    * not essential
;;; :ej - exit job   
;;; :bj - buffer job  
;;; :sm - start job on machine 
;;; :up - entry up   *           
;;; :bl - blocked
;;; :ub - unblocked
;;; :st - starved
;;; :us - unstarved
;;;=============== Actions update the model =====================================
(s/fdef bring-machine-up ; ; machine must be down (:future = :up)
        :args (s/and (s/cat :model ::Model :m keyword?)
                     #(let [m (:m %)]
                        (= :up (-> (:model %) :line m val val :future second)))))
        
(defn bring-machine-up 
  "Update :future on a down machine; log the action."
  [model m]
  (let [[n event etime] (-> model :line m :future)
        next-state (first (take 1 (drop-while #(<= (first %) n)
                                              (-> model :line m :up&down))))]
    (-> model
        (log/log {:clk etime :m m :act :up})
        (assoc-in [:line m :future] next-state))))

(s/fdef bring-machine-down ; machine must be up (:future = :down)
        :args (s/and (s/cat :model ::Model :m keyword?)
                     #(let [m (:m %)]
                        (= :down (-> (:model %) :line m val val :future second)))))

(defn bring-machine-down
  "Update :future on an up machine; log the action."
  [model m]
  (let [[n event etime] (-> model :line m :future)
        next-state (first (take 1 (drop-while #(<= (first %) n)
                                              (-> model :line m :up&down))))]
    (-> model
        (log/log {:clk etime :m m :act :down})
        (assoc-in [:line m :future] next-state))))

(defn add-job
  "Add a job to the line. Save the time it completes on the machine."
  [model j m]
  (let [now  (:clock model)
        ends (end-time model (util/job-requires model j m) m)
        job (-> j
                (assoc :enters (:clock model))
                (assoc :starts (:clock model))
                (assoc :ends ends))]
    (-> model
        (log/log {:act :aj :m m :j (:id job) :jt (:type job) :ends ends
                  :clk (:clock model) :dets (log/detail model)}) ; not essential
        (update-in [:params :current-job] inc)
        (assoc-in [:line m :status] job)
        (assoc-in [:line m :future] (machine-future model m)))))

(s/fdef advance2machine ; Check that buffer has a job. 
        :args (s/and (s/cat :model ::Model :m keyword?) ; jobs-move-to-down-machine?
                     #(let [m (:m %)
                            b-name (util/takes-from (:model %) m)]
                        (-> (:model %) :line b-name val :holding)))
        :ret ::Model)

(defn advance2machine
  "Move a job off the buffer onto the machine. Compute time it completes."
  [model m]
  (let [b-name (util/takes-from model m)
        b (-> model :line b-name)
        job (-> (first (:holding b))
                (assoc :starts (:clock model)))
        ends (end-time model (util/job-requires model job m) m)
        job (assoc job :ends ends)]
    (-> model 
        (log/log {:act :sm :m m :bf b-name :j (:id job) :n (count (:holding b))
                  :clk (:clock model) :dets (log/detail model)})
        (assoc-in  [:line m :status] job)
        (assoc-in  [:line m :future] (machine-future model m))
        (update-in [:line b-name :holding] #(vec (rest %))))))

(s/fdef advance2buffer
        :args (s/and (s/cat :model ::Model :m keyword?)
                     #(let [m (:m %)] ; second is to walk through [:machine [:emachine ...]]
                        (-> (:model %) :line m val val :status)))
        :ret ::Model)

(defn advance2buffer
  "Move a job to buffer." 
  [model m]
  (let [mach (-> model :line m)
        job  (:status mach)
        b?   (util/buffers-to model m)
        now  (:clock model)]
    (as-> model ?m
      (if b?
        (log/log ?m {:act :bj :m m :bf b? :j (:id job) :n (count (-> ?m :line b? :holding))
                     :clk now :dets (log/detail model)})
        (log/log ?m {:act :ej :m (:name mach) :j (:id job) :ent (:enters job) 
                     :clk now :dets (log/detail model)}))
      (assoc-in ?m [:line m :status] nil)
      (cond-> ?m
        b? (update-in [:line b? :holding] conj job)))))

;;;=============== End of Actions ===============================================
;;;=============== Record-actions don't move jobs around. =======================
(defn new-blocked
  [model m]
  (-> model
      (log/log {:act :bl :m m :clk (:clock model) :dets (log/detail model)})
      (update :blocked conj m)))

(defn new-unblocked
  [model m]
  (-> model
      (log/log {:act :ub :m m :clk (:clock model) :dets (log/detail model)})
      (update :blocked disj m)))

(defn new-starved
  [model m]
  (-> model 
      (log/log {:act :st :m m :clk (:clock model) :dets (log/detail model)})
      (update :starved conj m)))

(defn new-unstarved
  [model m]
  (-> model
      (log/log {:act :us :m m :clk (:clock model) :dets (log/detail model)})
      (update :starved disj m)))
;;;=============== End of Record-actions ========================================
(defn new-job
  "Randomly provide a new jobtype according to (:jobmix model)."
  [model]
  (let [choose (rand)
        type (loop [portions (map (fn [[k v]] [k (:portion v)]) (:jobmix model))
                    accum (second (first portions))]
               (when (not-empty portions)
                 (let [[jobtype portion] (first portions)]
                   (if (<= choose accum)
                     jobtype
                     (recur (rest portions)
                            (+ accum portion))))))]
    (map->Job {:type type :id (inc (:current-job (:params model)))})))

;;;===== Find runable actions  =============================
(defn add-job? 
  "Return a record-action to start a job on an entry-point machine."
  [model]
  (let [m (:entry-point model)
        mach (-> model :line m)]
    (when (and (not (util/occupied? mach))
               (not ((:blocked model) m)) ; stronger than bbs/buffer-full. Prevents :aj>:ub on same clock-tick. 
               (util/up? mach) ; 2018-01-27 at one point I didn't have this... (jm2dm?)
               (or (= :BAS (:discipline mach))
                   (and (= :BBS (:discipline mach))
                        (not (util/buffer-full? model mach)))))
      [{:time (:clock model) :fn add-job :args (list (new-job model) (:entry-point model))}])))

(defn bring-machine-up?
  "Return an action to bring a down machine up."
  [model]
  (let [bring-up (filter #(util/down? %) (machines model))]
    (when (not-empty bring-up) 
      (vec (map (fn [mn] {:time (nth (:future mn) 2)
                          :fn bring-machine-up :args (list (:name mn))})
                bring-up)))))

(defn bring-machine-down?
  "Return an action to bring an up machine down."
  [model]
  (let [bring-down (filter #(util/up? %) (machines model))]
    (when (not-empty bring-down) 
      (vec (map (fn [mn] {:time (nth (:future mn) 2)
                          :fn bring-machine-down :args (list (:name mn))})
                bring-down)))))

;;; Don't consider (> (:clock model) (-> mach :status :ends)) is too late!
;;; Any message, other than the one on the next clock tick, is ignored. 
(defn new-blocked-bas?
  "Return true if named machine is blocked BAS."
  [model m]
  (let [mach (-> model :line m)]
    (and
     (not ((:blocked model) m))
     (util/buffer-full? model mach)
     (util/occupied? mach))))
    
;;; Interesting that you don't even look at the downstream buffer here;
;;; that is done before starting a job. 
(defn new-blocked-bbs?
  "Return true if named machine is blocked BBS."
  [model m]
  (let [mach (-> model :line m)]
    (and
     (not ((:blocked model) m))
     (not (util/occupied? mach))
     (not (util/feed-buffer-empty? model mach)))))

(defn blocked-check-time
  "Most acts specify the time of occurrence; blocking can't. So it is done now."
  [model act]
  (let [m (-> act :args first)
        mach (-> model :line m)]
    (cond (= :BAS (-> model :line m :discipline))
          (if (and (util/buffer-full? model mach)
                   (util/occupied? mach))
            (-> model :line m :status :ends)
            ##Inf)
          (= :BBS (-> model :line m :discipline))
          (if (and (not (util/occupied? mach))
                   (not (util/feed-buffer-empty? model mach))
                   (util/buffer-full? model mach))
            (:clock model)
            ##Inf))))

(defn new-blocked?
  "Return record-actions to list newly blocked machines."
  [model]
  (let [clock (:clock model)
        old-block (:blocked model)
        new-block  (filter #(if (= :BAS (-> model :line % :discipline))
                              (new-blocked-bas? model %)
                              (new-blocked-bbs? model %))
                           (:machines model))]
    (when (not-empty new-block) 
      (vec (map (fn [m] {:time blocked-check-time ; that is, check when you see this.
                         :fn new-blocked :args (list m)}) new-block)))))

(defn new-unblocked-bas?
  [model m]
  (let [mach (-> model :line m)]
    (and ((:blocked model) m)
         (not (util/buffer-full? model mach)))))

;;; POD Same thing, I think! It's just that add-job/advance2machine that differ.
(defn new-unblocked-bbs?
  [model m]
  (let [mach (-> model :line m)]
    (and ((:blocked model) m)
         (not (util/buffer-full? model mach)))))

(defn new-unblocked?
  "Return record-actions to unlist certain machines as blocked."
  [model]
  (let [clock (:clock model)
        old-block (:blocked model)
        new-unblock (filter #(if (= :BAS (-> model :line % :discipline))
                               (new-unblocked-bas? model %)
                               (new-unblocked-bbs? model %))
                            (:machines model))]
    (when (not-empty new-unblock)
      (vec (map (fn [m] {:time clock :fn new-unblocked :args (list m)}) new-unblock)))))

(defn new-starved?
  [model]
  (let [clock (:clock model)
        new-starve  (filter #(and
                              (not ((:starved model) (:name %)))
                              (util/finished? model %)
                              (util/feed-buffer-empty? model %))
                            (machines model))]
    (when (not-empty new-starve)
      (vec (map (fn [mn] {:time clock :fn new-starved :args (list (:name mn))}) new-starve)))))

(defn new-unstarved?
  [model]
  (let [clock (:clock model)
        new-unstarve (filter #(and
                               ((:starved model) (:name %))
                               (not (util/feed-buffer-empty? model %)))
                             (machines model))]
    (when (not-empty new-unstarve)
      (vec (map (fn [mn] {:time clock :fn new-unstarved :args (list (:name mn))}) new-unstarve)))))

(defn advance2buffer?
  "Return actions to buffer and times at which these can occur (which may be later than clock)."
  [model]
  (let [blocked (:blocked model)
        advance (filter #(and
                          (util/occupied? %) ; maybe not finished yet; will be here.
                          (not (blocked (:name %)))  ; prevents :aj/sm>:bl on same clock-tick
                          (not (util/buffer-full? model %)))
                        (machines model))]
    (when (not-empty advance)
      (vec (map (fn [mn] {:time (max (:clock model) (:ends (:status mn))) ; Key idea!
                          :fn advance2buffer :args (list (:name mn))}) advance)))))

(defn advance2machine?
  "Return actions to move jobs onto machine. Can be done now." 
  [model]
  (let [entry-machine (:entry-point model)
        clock (:clock model)
        jm2dm? (:jm2dm model)
        blocked (:blocked model)
        starved (:starved model)
        advance (filter #(let [m (:name %)]
                           (and  ; Unless it was starved, :jm2dm doesn't matter.
                            (or (util/up? %) jm2dm?)
                            (not (= m entry-machine))
                            (not (blocked m)) ; stronger than bbs/buffer-full. Prevents :sm>:ub
                            (not (starved m)) ; Prevents :sm>:us. Both problems on same clock-tick.
                            (not (util/occupied? %))
                            (not (util/feed-buffer-empty? model %))
                            (or (= :BAS (:discipline %))
                                (and (= :BBS (:discipline %))
                                     (not (util/buffer-full? model %))))))
                        (machines model))]
    (when (not-empty advance)
      (vec (map (fn [mn] {:time clock :fn advance2machine :args (list (:name mn))}) advance)))))

(defn get-time
  "(-> act :time) may be a number or a function that returns one."
  [model act]
  (if (number? (:time act))
    (:time act)
    ((:time act) model act)))

(defn runables
  "Return a sequence of all the actions and record-actions 
   that can occur in the next time increment (all have = time)."
  [model]
  (let [all (concat ; none of the following modify the model. 
             (add-job? model)
             (bring-machine-up? model)
             (bring-machine-down? model)
             (advance2buffer? model)
             (advance2machine? model) 
             (new-starved? model)  
             (new-unstarved? model)  
             (new-blocked? model)  
             (new-unblocked? model))]
    (when (not-empty all)
      (let [min-time (apply min (map #(get-time model %) all))]
        (filter #(= (get-time model %) min-time) all)))))
      
(defn run-action
  "Run a single action, returning the model."
  [model act]
  (apply (partial (:fn act) model) (:args act)))

(defn spec-check-model
  "Do clojure.spec-based testing of the model."
  [model]
  (if (s/valid? ::Model model)
    model
    (throw
     (ex-info "The model is not well-formed:"
              {:reason (s/explain ::Model model)}))))

(defn rand-range
  [[lbound ubound]]
  (+ lbound (* (rand) (- ubound lbound))))

(defn preprocess-equip
  [[k e]]
  (cond (instance? ExpoMachine e)
        (let [lambda (:lambda e)
              mu     (:mu     e)
              discipline (or (:discipline e) :BAS)
              first-event (expo-up&down-init-event lambda mu)]
          (as-> e ?m 
            (assoc ?m :name k)
            (assoc ?m :discipline discipline) ; POD end machine shouldn't have a discipline. 
            (assoc ?m :up&down  (expo-up&down lambda mu first-event))
            (assoc ?m :future first-event)
            (assoc ?m :W (if (number? (:W ?m)) (:W ?m) (rand-range (:bounds (:W ?m)))))
            [k ?m])),
        (instance? ReliableMachine e)
        (as-> e ?m 
          (assoc ?m :future [:down Double/POSITIVE_INFINITY])
          (assoc ?m :name k)
          [k ?m]),
        (instance? Buffer e)
        (as-> e ?b
          (assoc ?b :holding [])
          (assoc ?b :name k)
          [k ?b]),
        :else [k e]))

(defn preprocess-model
  "Add detail and check model for correctness. 
   Make line a sorted-map (if only for readability)."
  [model & {:keys [check?] :or {check? true}}]
  (let [jm2dm (or (:jm2dm model) (:jobs-move-to-down-machines? +defaults+))] ; POD fix this?
    (as-> model ?m
      (assoc ?m :log-buf [])
      (assoc ?m :diag-log-buf [])
      (update-in ?m [:report] #(if (empty? %)
                                 {:log? false :line-cnt 0 :max-lines 0}
                                 (assoc % :line-cnt 0)))
      (assoc ?m :jm2dm jm2dm)
      (assoc ?m :line (into (sorted-map) (map preprocess-equip (:line model))))
      (assoc ?m :machines (filterv #(machine? (-> model :line %)) (:topology model)))
      (assoc ?m :buffers  (filterv #(buffer?  (-> model :line %)) (:topology model)))
      (assoc ?m :clock 0.0)
      (assoc ?m :blocked #{})
      (assoc ?m :starved #{})
      (if (contains? (:params ?m) :time-format)
        ?m
        (assoc-in ?m [:params :time-format] (:time-format +defaults+)))
      (assoc ?m :jobmix ; set jobtype :w values, which may be differ stochastically for each simulation
             (reduce (fn [jmix jt-key] 
                       (assoc jmix jt-key
                              (assoc (jt-key jmix)
                                     :w
                                     (reduce-kv (fn [m k v] 
                                                  (assoc m k (if (number? v) v (rand-range (:bounds v)))))
                                                (:w (jt-key jmix))     ;init
                                                (:w (jt-key jmix)))))) ;collection
                     (:jobmix ?m) (keys (:jobmix ?m))))
      (assoc-in ?m [:params :current-job] 0)
      (cond-> ?m check? spec-check-model))))

(defn calc-basics
  "Produce a results form containing percent time blocked, and job residence time."
  [model log]
  (let [warm-up-time (:warm-up-time (:params model))
        run-time (- (:run-to-time (:params model)) warm-up-time)
        residence-sum (:residence-sum log)
        njobs (:njobs log)]
    {:runtime run-time
     :TP (float (/ njobs run-time))
     :observed-residence-time (if (= 0 njobs) :na (/ residence-sum njobs))
     :number-of-jobs njobs
     :blocked (into {} (map #(vector % (/ (:blocked (% log)) run-time)) (:machines model)))
     :starved (into {} (map #(vector % (/ (:starved (% log)) run-time)) (:machines model)))
     :avg-buffer-occupancy (into {} (map (fn [bf] (vector bf (/ (apply + (map (fn [n t] (* n t))
                                                                          (keys (dissoc (bf log) :lastclk))
                                                                          (vals (dissoc (bf log) :lastclk))))
                                                            run-time)))
                                     (:buffers model)))}))

(defn calc-bneck
  "Update results with identification of bottlenecks."
  [res model]
  (let [^clojure.lang.PersistentVector m-order (:machines model)]
    (letfn [(mIndex [m] (inc (.indexOf m-order m)))
            (bl [i] (nth (vals (:blocked res)) (dec i)))
            (st [i] (nth (vals (:starved res)) (dec i)))]
      (let [candidates (filter #(when (not= % (last m-order))
                                  (< (bl (mIndex %)) (st (inc (mIndex %))))) m-order)]
        ;(println ";Candidates =" candidates)
        (if (= 1 (count candidates))
          (assoc res :bottleneck-machine (first candidates))
          (let [mcnt  (count m-order)
                ^clojure.lang.LazySeq severity
                (zipmap (:machines model)
                        (map (fn [i]
                               (cond
                                 (= i 1) (Math/abs (- (bl 1) (st 2)))
                                 (= i mcnt) (Math/abs (- (bl (dec mcnt)) (st mcnt)))
                                 :else   (+ (Math/abs (- (bl (dec i)) (st i)))
                                            (Math/abs (- (bl i) (st (inc i)))))))
                             (range 1 (inc mcnt))))]
            ;(println ";severity = " severity)
            (let [max-val (apply max (vals severity))]
              (assoc res :bottleneck-machine
                     (first (first (filter (fn [[k v]] (= v max-val)) severity)))))))))))

(defn calc-residence-time ; POD Needs work!
  "Calculate job residence time as function of processing time, and :buffer-residency, and BL."
  [res model]
  (let [mach-effs (zipmap
                   (:machines model)
                   (map (fn [mn]
                          (let [m (-> model :line mn)]
                            (/ (:mu m)  (+ (:lambda m) (:mu m)))))
                        (:machines model))),
        virt-processing
        (zipmap
         (:machines model)
         (map (fn [m]
                (apply + (map (fn [jt]
                                (* (:portion (jt (:jobmix model)))
                                   (/ 1.0 (m mach-effs))
                                   (util/job-requires model (map->Job {:type jt}) m)))
                              (keys (:jobmix model)))))
              (:machines model)))]
    (assoc res :computed-residence-time
           (zipmap
            (keys (:jobmix model))
            (map (fn [jtype]
                   (let [job (map->Job {:type jtype})]
                     (+  (apply + ; processing time
                                (map (fn [m] (* (/ 1.0 (m mach-effs))
                                                 (+ 1.0 (m (:blocked res)))
                                                 (util/job-requires model job m)))
                                     (:machines model)))
                         (apply + ; wait time. 
                                (map (fn [m]
                                       (if (= m (:entry-point model))
                                         0 ; 0.5 (below) is half of vjob being worked.
                                         (* (+ 0.5 ((util/takes-from model m) (:avg-buffer-occupancy res)))
                                            (m virt-processing)
                                            (+ 1.0 (m (:blocked res))))))
                                     (:machines model))))))
                 (keys (:jobmix model)))))))

(defn analyze-results [model log]
  "Read the w-<date> output file and compute results."
    (as-> (calc-basics model log) ?res
      (calc-bneck ?res model)
      #_(calc-residence-time ?res model)))

(defn postprocess-model
  "Create a results object."
  [model n start]
  (as-> (analyze-results model @log/*log-steady*) ?r
    (assoc ?r :process-id n)
    (if (-> model :report :diag-log-buf?)
      (assoc ?r :diag-log-buf (:diag-log-buf model))
      ?r)
    (assoc ?r :jobmix (:jobmix model))
    (assoc ?r :status (:status (:params model)))
    (assoc ?r :runtime (/ (- (System/currentTimeMillis) start) 1000.0))))

(def +kill-all+ (atom false))

(defn end-main-loop?
  [model job-end time-end]
  (not (or (and (:run-to-job (:params model))
                (<= (:current-job (:params model)) job-end))
           (and (:run-to-time (:params model))
                (<= (:clock model) time-end)))))

(defn main-loop-loop
  "Body of action loop that does the work"
  [model job-end time-end]
  (reset! +kill-all+ false)
  (loop [model model]
    (if (end-main-loop? model job-end time-end)
      (assoc-in model [:params :status] :normal-end)
      (let [actions  (when-not @+kill-all+ (runables model))
            next-clk (when actions (get-time model (first actions)))]
        (if (empty? actions) 
          (assoc-in model [:params :status] :no-runables)
          (as-> model ?m
            (advance-clock ?m next-clk)
            (reduce #(run-action %1 %2) ?m actions)
            (log/push-log ?m)
            (recur ?m)))))))

(defn main-loop-multi
  "Run several simulations."
  [model out-stream]
  (binding [*out* out-stream
            *print-length* 10]
    (print "#_")
    (pprint (log/pretty-model model))
    (let [sims ; a seq of futures.
          (map
           (fn [n]
             (future
               (let [start (System/currentTimeMillis)
                     job-end  (:run-to-job  (:params model))
                     time-end (:run-to-time (:params model))]
                   (as-> model ?m
                       (preprocess-model ?m)
                       (binding [log/*log-steady* (ref (log/steady-form ?m))] ; create a log for computations.
                         (-> ?m
                             (main-loop-loop job-end time-end)
                             (postprocess-model n start))))))) ; Returns a result object
           (range (or (:number-of-simulations model) 1)))]
      (log/output-sims model sims))))
  
(defn main-loop-once
  "Run one simulation."
  [model out-stream]
  (print "#_")
  (pprint (log/pretty-model model))
  (binding [*out* out-stream
            *print-length* 10]
    (let [start (System/currentTimeMillis)
          job-end  (:run-to-job  (:params model))
          time-end (:run-to-time (:params model))]
      (as-> model ?m
        (preprocess-model ?m)
        (binding [log/*log-steady* (ref (log/steady-form ?m))] ; create a log for computations.
          (as-> ?m ?m2
            (main-loop-loop ?m2 job-end time-end)
            (postprocess-model ?m2 1 start))) ; Returns a result object
          (do (print "#_") ?m)
          (-> ?m (dissoc :jobmix) pprint)))))

(defn main-loop
  "Run one or more simulations."
  [model & {:keys [out-stream] :or {out-stream *out*}}]
  (let [n (:number-of-simulations model)]
    (if (or (not n) (= n 1))
      (main-loop-once  model out-stream)
      (main-loop-multi model out-stream))))

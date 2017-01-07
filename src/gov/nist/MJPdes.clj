(ns gov.nist.MJPdes
  (:require [medley.core :refer (abs)]
            [incanter.stats :as s :refer (sample-exp)]
            [clojure.pprint :refer (cl-format pprint)]
            [clojure.edn :as edn]))

;;; Purpose: Implements a discrete event simulation engine for multi-job production.

;;; Q: 
;;;   - BLOCKED means:
;;;        - There is a completed job that can't move to the buffer OR 
;;;        - The above plus the machine is not starved ?
;;; A: Doesn't matter much with these models, but if you had a machine that
;;;    had much greater capacity than the others, it would starve often.
;;;    I would not consider it blocked when it also going to be starved when
;;;    space in the buffer eventually is available. 
;;;   - fix blocked-requires-not-starved

(def +default-jobs-move-to-down-machines+ false)
(def +diag+ (atom nil))
(def +warm-up-time+ (atom nil))
;(def +diag-expect-job+ (atom 0))

(defrecord Model [line entry-point jobmix params topology])

(defrecord Machine [])
(defrecord ReliableMachine [name status])  ; For specific tests. Assume W=1
(defrecord ExpoMachine     [name status W mchain])

(defn reliable? [m]
  (= ReliableMachine (type m)))

(defn machine? [m]
  (let [t (type m)]
    (or (= ExpoMachine t) (= ReliableMachine t) (= Machine t))))

(defn machines [model]
  (let [line (:line model)]
    (map #(% line) (:machines model))))

(defrecord Buffer [N holding])
(defrecord InfiniteBuffer [])
(defrecord DedicatedBuffer [])

(defn buffer? [b]
  (let [t (type b)]
    (or (= Buffer t) (= InfiniteBuffer t) (= DedicatedBuffer t))))

(defrecord JobType [w])

(defrecord Job [type
                id
                enters ; clock when entered on FIRST MACHINE
                starts ; clock at which it will start CURRENT MACHINE
                ends]) ; clock at which it will finish CURRENT MACHINE

(defn lookup
  "Return the machine or buffer named NAME."
  [model name]
  (get (:line model) name))

(defn buffers-to
  "Return the name of the buffer that the named machine buffers to.
   (Returns the thing after the argument.)"
  [model m-name]
  (let [^clojure.lang.PersistentVector top (:topology model)]
    (when-let [pos (.indexOf top m-name)]
      (when (< pos (dec (count top)))
        (nth top (inc pos))))))

(defn takes-from
  "Return the buffer that the named machine takes work from."
  [model m-name]
  (let [^clojure.lang.PersistentVector top (:topology model)]
    (when-let [pos (.indexOf top m-name)]
      (when (> pos 0)
        (nth top (dec pos))))))

(defn up? [m]
  (= :down (first (:future m))))

(defn down? [m]
  (= :up (first (:future m))))

(defn finished? [model m]
  (let [status (:status m)]
    (or (not status)
        (>= (:clock model) (:ends status)))))

(defn occupied? [m]
  (:status m))

(defn feed-buffer-empty? [model m] 
  "Returns true if buffer feeding machine m is empty." 
  (when (not= (:name m) (:entry-point model))
    (let [buf (lookup model (takes-from model (:name m)))]
      (= (count (:holding buf)) 0))))

(defn buffer-full? [model m] 
  "Returns true if the buffer that machine m places completed work on is full."
  (when-let [buf (lookup model (buffers-to model (:name m)))] ; last machine cannot be blocked.
    (= (count (:holding buf)) (:N buf))))

(defn job-requires
  "Total time that job j requires on a machine m, (w_{ij}/W_i)"
  [model j m]
  (let [W (or (:W m) 1.0)
        w (get (:w (get (:jobmix model) (:type j))) (:name m))] ; POD -> 
    (/ w W)))

(defn catch-up-machine!
  "Advance the machine state so that its :future is in the future." 
  [m now]
  (let [[_ ftime] (:future m)]
    (if (> ftime now)
      m
      (assoc m :future
             (loop [fut (:future m)]
               (let [[_ ftime] fut]
                    (if (> ftime now) 
                      fut
                      (recur ((:mchain m))))))))))

(defn advance-clock
  "Move clock and machines forward in time."
  [model new-clock]
  (let [clock (:clock model)]
    (cond
      (> clock new-clock)
      (throw (ex-info "Clock moving backwards!" {:clock clock :new-clock new-clock})),
      (= clock new-clock)
      model
      :else
      (as-> model ?m
        (reduce (fn [mod m-name]
                  (assoc-in mod [:line m-name]
                            (catch-up-machine! (lookup model m-name) new-clock)))
                ?m (:machines ?m))
        (assoc ?m :clock new-clock)))))

(defn job-updates!
  "Advance through ups and downs collecting time for job.  
   Return end time and future state, which should be a [:down <time>]."
  [model job m]
  (let [dur (job-requires model job m)
        start (:clock model)] ; Assumes job starts now (startable or +job-moves-...)
    (loop [decum dur
           now start ; used as start of a working period to collect from
           future (:future m)]
      (let [[state dw_time] future]
        (if (= :up state)
          (recur decum dw_time ((:mchain m)))
          (if (>= (- dw_time now) decum)
            [(+ now decum) future]
            (recur (- decum (- dw_time now))
                   dw_time
                   ((:mchain m)))))))))

(declare log)

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
(defn bring-machine-up
  "Nothing to do here but (possible) loggging. The clock was advanced and 
   machine futures updated by call to advance-clock in main-loop."
  [model m-name]
  ;(let [m (lookup model m-name)] ; diag
    ;(when (up? m-name) (throw (ex-info "Machine is already up!" {:machine m-name})))
    ;(log {:act :up :m m-name :clk (:clock model)}) ; not essential
    model)

(defn add-job
  "Add a job to the line. Save the time it completes on the machine."
  [model j m-name]
  (let [[ends future] (job-updates! model
                                   (assoc j :starts (:clock model))
                                   (lookup model m-name))
        job (as-> j ?j
              (assoc ?j :enters (:clock model))
              (assoc ?j :starts (:clock model))
              (assoc ?j :ends ends))]
    ;(log {:act :aj :j (:id job) :jt (:type job) :ends ends :clk (:clock model)}) ; not essential
    (-> model
        (update-in [:params :current-job] inc)
        (assoc-in [:line m-name :status] job)
        (assoc-in [:line m-name :future] future))))

(defn advance2machine
  "Move a job off the buffer onto the machine. Compute time it completes."
  [model m-name]
;;;  (when (:status (lookup model m-name)) ; diag
;;;    (throw (ex-info "Machine already busy!" {:machine m-name :model model})))
  (let [b-name (takes-from model m-name)
        b (lookup model b-name)
        job (-> (first (:holding b))
                (assoc :starts (:clock model)))
        [ends future] (job-updates! model job (lookup model m-name))
        job (assoc job :ends ends)]
;;;    (when (= :up (first future))
;      (throw (ex-info "Expected an down future!" {:m-name m-name :model model})))
    (log {:act :sm :bf b-name :j (:id job) ;:ends ends
          :n (count (:holding b)) :clk (:clock model)})
    (as-> model ?m
      (assoc-in ?m  [:line m-name :status] job)
      (assoc-in ?m  [:line m-name :future] future)
      (update-in ?m [:line b-name :holding] #(vec (rest %))))))

(defn advance2buffer
  "Move a job to buffer." 
  [model m-name]
  (let [m (lookup model m-name)
        job (:status m)
        b? (buffers-to model m-name)
        now (:clock model)]
;    (when (not b?) 
;      (when (not= @+diag-expect-job+ (:id job))
;        (throw (ex-info "Jobs out of order!" {:model model})))
;    (swap! +diag-expect-job+ inc))
;    (when (and b? (= (count (:holding b?)) (:N b?))) ; diag
;      (throw (ex-info "buffer exceeds capacity!" {:model model :m-name :m-name})))
    (if b?
      (log {:act :bj :bf b? :j (:id job) :n (count (:holding (lookup model b?))) :clk now}) 
      (log {:act :ej :m (:name m) :j (:id job) :ent (:enters job) :clk now}))
    (as-> model ?m
      (assoc-in ?m [:line m-name :status] nil)
      (if b?
        (update-in ?m [:line b? :holding] conj job)
        ?m))))

;;;=============== End of Actions ===============================================
;;;=============== Record-actions don't move jobs around. =======================
(defn new-blocked
  [model m-name]
  (log {:act :bl :m m-name :clk (:clock model)})
  (update model :blocked conj m-name))

(defn new-unblocked
  [model m-name]
  (log {:act :ub :m m-name :clk (:clock model)})
  (update model :blocked disj m-name))

(defn new-starved
  [model m-name]
  (log {:act :st :m m-name :clk (:clock model)})
  (update model :starved conj m-name))

(defn new-unstarved
  [model m-name]
  (log {:act :us :m m-name :clk (:clock model)})
  (update model :starved disj m-name))
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
  (let [e (lookup model (:entry-point model))]
    (when (not (occupied? e)) ; Doesn't matter if up or down.
      [{:time (:clock model) :fn add-job :args (list (new-job model) (:entry-point model))}])))

(defn bring-machine-up?
  "Return an action to bring a down machine up."
  [model]
  (let [bring-up (filter #(down? %) (machines model))]
    (when (not-empty bring-up) ; second of two places time advances.
      (vec (map (fn [mn] {:time (second (:future mn)) :fn bring-machine-up :args (list (:name mn))})
                bring-up)))))

(defn new-blocked?
  "Return record-actions to list newly blocked machines."
  [model]
  (let [clock (:clock model)
        old-block (:blocked model)
        new-block  (filter #(and 
                             (buffer-full? model %)
                             (not (contains? old-block (:name %)))
                             (occupied? %) 
                             (finished? model %)) 
                           (machines model))]
    (when (not-empty new-block)
      (vec (map (fn [mn] {:time clock :fn new-blocked :args (list (:name mn))}) new-block)))))

(defn new-unblocked?
  "Return record-actions to unlist certain machines as blocked."
  [model]
  (let [clock (:clock model)
        old-block (:blocked model)
        new-unblock (filter #(and
                              (contains? old-block (:name %))
                              (not (buffer-full? model %)))
                            (machines model))]
    (when (not-empty new-unblock)
      (vec (map (fn [mn] {:time clock :fn new-unblocked :args (list (:name mn))}) new-unblock)))))

(defn new-starved?
  "Return record-actions to add starved machines."
  [model]
  (let [clock (:clock model)
        old-starve (:starved model)
        new-starve (filter #(and 
                              (finished? model %)
                              (not (contains? old-starve (:name %)))
                              (feed-buffer-empty? model %))
                            (machines model))]
    (when (not-empty new-starve)
      (vec (map (fn [mn] {:time clock :fn new-starved :args (list (:name mn))}) new-starve)))))

(defn new-unstarved?
  "Return record-actions to unlist certain machines as starved."
  [model]
  (let [clock (:clock model)
        old-unstarve (:starved model)
        new-unstarve (filter #(and
                                (contains? old-unstarve (:name %))
                                (not (feed-buffer-empty? model %)))
                              (machines model))]
    (when (not-empty new-unstarve)
      (vec (map (fn [mn] {:time clock :fn new-unstarved :args (list (:name mn))}) new-unstarve)))))

(defn advance2buffer?
  "Return actions to buffer and times at which these can occur (which may be later than clock)."
  [model]
  (let [advance (filter #(and
                          (occupied? %) ; maybe not finished yet; will be here. 
                          (not (buffer-full? model %)))
                        (machines model))]
    (when (not-empty advance)
      (vec (map (fn [mn] {:time (max (:clock model) (:ends (:status mn))) ; Key idea!
                          :fn advance2buffer :args (list (:name mn))}) advance)))))

(defn advance2machine?
  "Return actions to move jobs onto machine. Can be done now." 
  [model]
  (let [entry-machine (:entry-point model)
        clock (:clock model)
        advance (filter #(and                
                          (or (up? %) (:jm2dm model))
                          (not (= (:name %) entry-machine))
                          (not (occupied? %))
                          (not (feed-buffer-empty? model %)))
                        (machines model))]
    (when (not-empty advance)
      (vec (map (fn [mn] {:time clock :fn advance2machine :args (list (:name mn))}) advance)))))

(defn runables
  "Return a sequence of actions and record-actions and 
   the time at which they can run."
  [model]
  (let [all (concat ; none of the following modify the model. 
             (add-job? model)
             (bring-machine-up? model)
             (advance2buffer? model)
             (advance2machine? model) 
             (new-starved? model)  
             (new-unstarved? model)  
             (new-blocked? model)  
             (new-unblocked? model))]
    (when (not-empty all)
      (let [min-time (apply min (map :time all))]
        (filter #(= (:time %) min-time) all)))))
      
(defn run-action
  "Run a single action, returning the model."
  [model act]
  ;(reset! +diag+ {:where 'run-action :act act})
  (apply (partial (:fn act) model) (:args act)))

(defn run-actions
  "Run a list of actions in the order they appear in actions; 
   update the model while doing so."
  [model actions]
  (as-> model ?m
    (reduce (fn [m a] (run-action m a)) ?m actions)
    (reset! +diag+ ?m)))

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
(defn exponential-up&down
  "Return a function that, when called, returns the next event and time 
   on a markov chain representing an exponential machine."
  [lambda mu]
  (let [up? (atom (< (rand) (/ mu (+ lambda mu)))) 
        T (atom (s/sample-exp 1 :rate (if @up? lambda mu)))
        T-delta (atom 0.0)]
    (fn []
      (loop [up-now? @up? 
             t-now @T]
        (let [t-delt (s/sample-exp 1 :rate (if up-now? mu lambda))]
          (if (< (rand) (p-state-change @T-delta (if up-now? lambda mu)))
            (let [answer [(if up-now? :down :up) t-now]]
              (reset! T-delta 0.0)
              (swap! T + t-delt)
              (swap! up? not)
              answer)
            (do (swap! T-delta + t-delt)
                (recur @up? @T))))))))

;;; POD: try to do this with Clojure.spec.
(defn check-model
  "Reports errors and returns false if bugs in model."
  [model]
  model)

(defn preprocess-equip
  [[k e]]
  (cond (= (type e) ExpoMachine)
        (as-> e ?m 
          (assoc ?m :mchain (exponential-up&down (:lambda ?m) (:mu ?m)))
          (assoc ?m :future ((:mchain ?m)))
          (assoc ?m :name k)
          ;(dissoc ?m :lambda)
          ;(dissoc ?m :mu)
          [k ?m]),
        (= (type e) ReliableMachine)
        (as-> e ?m 
          (assoc ?m :future [:down Double/POSITIVE_INFINITY])
          (assoc ?m :name k)
          [k ?m]),
        (= (type e) Buffer)
        (as-> e ?b
          (assoc ?b :holding [])
          (assoc ?b :name k)
          [k ?b])
        :else [k e]))

(defn preprocess-model
  "Add detail and check model for correctness. 
   Make line a sorted-map (if only for readability)."
  [model]
  (reset! +warm-up-time+ (:warm-up-time (:params model)))
  (let [jm2dm (or (:jm2dm model) +default-jobs-move-to-down-machines+)]
    (-> model 
        (check-model)
        (assoc :jm2dm jm2dm)
        (assoc :line (into (sorted-map) (map preprocess-equip (:line model))))
        (assoc :machines (vec (filter #(machine? (lookup model %)) (:topology model))))
        (assoc :buffers  (vec (filter #(buffer?  (lookup model %)) (:topology model))))
        (assoc :clock 0.0)
        (assoc :blocked #{})
        (assoc :starved #{})
        (assoc-in [:params :current-job] 0))))

(def +log+ (atom nil))

(defn log-form! 
  "Side-effect:Create a results for map. Return model untouched."
  [model]
  (reset! +log+ 
          (as-> {:residence-sum 0.0, :njobs 0} ?r
            (into ?r (map (fn [m] [m {:blocked 0.0 :starved 0.0 :bs nil :ss nil}])
                          (:machines model)))
            (into ?r (map (fn [b] [(:name b) (assoc (zipmap (range (inc (:N b))) (repeat (+ 2 (:N b)) 0.0))
                                                    :lastclk (:warm-up-time (:params model)))])
                          (map #(lookup model %) (:buffers model))))))
  model)

(defn calc-basics
  "Produce a results form containing percent time blocked, and job residence time."
  [log model]
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
        (println "Candidates =" candidates)
        (if (= 1 (count candidates))
          (assoc res :bottleneck-machine (first candidates))
          (let [mcnt  (count m-order)
                ^clojure.lang.LazySeq severity
                (zipmap (:machines model)
                        (map (fn [i]
                               (cond
                                 (= i 1) (abs (- (bl 1) (st 2)))
                                 (= i mcnt) (abs (- (bl (dec mcnt)) (st mcnt)))
                                 :else   (+ (abs (- (bl (dec i)) (st i)))
                                            (abs (- (bl i) (st (inc i)))))))
                             (range 1 (inc mcnt))))]
            (println "severity = " severity)
            (let [max-val (apply max (vals severity))]
              (assoc res :bottleneck-machine
                     (first (first (filter (fn [[k v]] (= v max-val)) severity)))))))))))

(defn calc-residence-time ; POD Needs work!
  "Calculate job residence time as function of processing time, and :buffer-residency, and BL."
  [res model]
  (let [mach-effs (zipmap
                   (:machines model)
                   (map (fn [mn]
                          (let [m (lookup model mn)]
                            (/ (:mu m)  (+ (:lambda m) (:mu m)))))
                        (:machines model))),
        virt-processing
        (zipmap
         (:machines model)
         (map (fn [mn]
                (apply + (map (fn [jt]
                                (* (:portion (jt (:jobmix model)))
                                   (/ 1.0 (mn mach-effs))
                                   (job-requires model (map->Job {:type jt}) (lookup model mn))))
                              (keys (:jobmix model)))))
              (:machines model)))]
    (assoc res :computed-residence-time
           (zipmap
            (keys (:jobmix model))
            (map (fn [jtype]
                   (let [job (map->Job {:type jtype})]
                     (+  (apply + ; processing time
                                (map (fn [mn] (* (/ 1.0 (mn mach-effs))
                                                 (+ 1.0 (mn (:blocked res)))
                                                 (job-requires model job (lookup model mn))))
                                     (:machines model)))
                         (apply + ; wait time. 
                                (map (fn [mn]
                                       (if (= mn (:entry-point model))
                                         0 ; 0.5 (below) is half of vjob being worked.
                                         (* (+ 0.5 ((takes-from model mn) (:avg-buffer-occupancy res)))
                                            (mn virt-processing)
                                            (+ 1.0 (mn (:blocked res))))))
                                     (:machines model))))))
                 (keys (:jobmix model)))))))

;;; Examples
;;;   r = {... :b2 {0 0.0, 1 0.0, 2 0.0, 3 0.0, 4 0.0, 5 0.0 :lastclk 0.0}},
;;;   o = {:act :bj, :bf :b1, :j 1, :n 0 :clk 0.833}
;;;   o = {:act :sm, :bfm :b1 :j 1, :ends 1.833, :n 0, :clk 0.833} ; n is size before this action.
;;; BTW, at the end, these should sum to run-time. 
(defn buf+ ; action is :bj
  "Update results tracking time at the buffer size. Used with :bj (move of machine)." 
  [r o]
  (-> r 
      (update-in [(:bf o) (:n o)] + (- (:clk o) (:lastclk ((:bf o) r))))
      (assoc-in  [(:bf o) :lastclk] (:clk o)))) ; This ends the clock 
  
(defn buf- ; :sm is not called for :m1
  "Update results tracking time at the buffer size. Used with :sm (start on machine)." 
  [r o]
  (-> r ; :n value is from before the add. 
      (update-in [(:bf o) (:n o)] + (- (:clk o) (:lastclk ((:bf o) r))))
      (assoc-in [(:bf o) :lastclk] (:clk o)))) ; This ends the clocl

(defn block+ ; action is :bl
  "Start clock on machine for blocking."
  [r o]
  ;(when (:bs ((:m o) r)) (throw (ex-info "Blocking twice!" {:r r :o o}))) ; diag
  (assoc-in r [(:m o) :bs] (:clk o)))

(defn block- ; action is :ub
  "Stop clock on machine for blocking; gather accumulated time."
  [r o]
  (if-let [bs (:bs ((:m o) r))]
    (-> r
        (update-in [(:m o) :blocked] + (- (:clk o) bs))
        (assoc-in  [(:m o) :bs] nil))
    r))

(defn starve+ ; action is :st
  "Start clock on machine for starving."
  [r o]
;  (when (:ss ((:m o) r)) (throw (ex-info "Starving twice!" {:r r :o o}))) ; diag
  (assoc-in r [(:m o) :ss] (:clk o)))

(defn starve- ; action is :us
  "Stop clock on machine for starving."
  [r o]
  (if-let [ss (:ss ((:m o) r))]
    (-> r
        (update-in [(:m o) :starved] + (- (:clk o) ss))
        (assoc-in  [(:m o) :ss] nil))
    r))

(defn end-job
  "Collect residence time for job ending."
  [r o]
  (-> r
      (update-in [:residence-sum] + (- (:clk o) (:ent o)))
      (update-in [:njobs] inc)))

(defn log [log-map]
  (when (> (:clk log-map) @+warm-up-time+)
    (swap! +log+
           #(case (:act log-map)
              :bj (buf+    % log-map)
              :sm (buf-    % log-map)
              :ej (end-job % log-map)
              :bl (block+  % log-map) 
              :ub (block-  % log-map)
              :st (starve+ % log-map)
              :us (starve- % log-map)))))

(defn analyze-results [log model]
  "Read the w-<date> output file and compute results."
    (as-> (calc-basics log model) ?res
      (calc-bneck ?res model)
      #_(calc-residence-time ?res model)))

(defn main-loop 
  "Run a simulation."
  [model]
  (let [start (System/currentTimeMillis)
        job-end  (:run-to-job  (:params model))
        time-end (:run-to-time (:params model))]
    (as-> model ?m
      (preprocess-model ?m)
      (log-form! ?m) ; create +log+ return model. 
      (loop [model ?m]
        (if-let [actions (not-empty (runables model))]
          (let [model (advance-clock model (:time (first actions)))]
            (if (or (and (:run-to-job (:params model))
                         (<= (:current-job (:params model)) job-end))
                    (and (:run-to-time (:params model))
                         (<= (:clock model) time-end)))
              (recur (run-actions model actions))
              (assoc-in model [:params :status] :normal-end)))
          (assoc-in model [:params :status] :no-runables)))
      (-> (analyze-results @+log+ ?m)
          (assoc :status (:status (:params ?m)))
          (assoc :runtime (/ (- (System/currentTimeMillis) start) 1000.0))))))

(def f1
  (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 }) 
     :b1 (map->Buffer {:N 3})
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
     :b2 (map->Buffer {:N 5})
     :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
     :b3 (map->Buffer {:N 1})
     :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
     :b4 (map->Buffer {:N 1})
     :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })}
    :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
    :entry-point :m1 ; 
    :params {:warm-up-time 2000 :run-to-time 20000}  
    :jobmix {:jobType1 (map->JobType {:portion 1.0
                                      :w {:m1 1.0, :m2 1.0, :m3 2.0, :m4 1.0, :m5 1.0}})}}))

(def f2
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










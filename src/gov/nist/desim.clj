(ns gov.nist.desim
  (:require [medley.core :refer :all]
            [incanter.stats :as s]
            [clojure.core.memoize :as m]
            [clojure.pprint :refer (cl-format pprint)]
            [clojure.edn :as edn]))


;;; Date: 2016-10-08
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

(def +default-jobs-move-to-down-machines+ false)  ; default value. SHOULD DEFAULT TO false.
(def +diag+ (atom nil))
(def +warm-up-time+ (atom nil))
;(def +diag-expect-job+ (atom 0))

(defrecord Model [line entry-point jobmix execute topology])

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
  (let [top (:topology model)]
    (when-let [pos (.indexOf top m-name)]
      (when (< pos (dec (count top)))
        (nth top (inc pos))))))

(m/memo buffers-to)

(defn feeds
  "Same as buffers-to, but name is meaningful for buffers."
  [model b-name]
  (buffers-to model b-name))

(m/memo feeds)

(defn takes-from
  "Return the buffer that the named machine takes work from."
  [model m-name]
  (let [top (:topology model)]
    (when-let [pos (.indexOf top m-name)]
      (when (> pos 0)
        (nth top (dec pos))))))

(m/memo takes-from)
      
(defmacro up? [m]
  `(= :down (first (:future ~m))))

(defmacro down? [m] ; inline
  `(= :up (first (:future ~m))))

(defmacro finished? [model m] ; inline
  `(let [status# (:status ~m)]
     (or (not status#)
         (>= (:clock ~model) (:ends status#)))))

(defmacro occupied? [m] ; inline
  `(:status ~m))

(defmacro feed-buffer-empty? [model m] ; inline
  "Returns true if buffer feeding machine m is empty." 
  `(if (= (:name ~m) (:entry-point ~model))
     false
     (let [buf# (lookup ~model (takes-from ~model (:name ~m)))]
       (= (count (:holding buf#)) 0))))

(defmacro buffer-full? [model m] ; inline
  "Returns true if the buffer that machine m places completed work on is full."
  `(if-let [buf# (lookup ~model (buffers-to ~model (:name ~m)))] ; last machine cannot be blocked.
     (= (count (:holding buf#)) (:N buf#))
     false))

(defn job-requires
  "Total time that job j requires on a machine m, (w_{ij}/W_i)"
  [model j m]
  (let [W (or (:W m) 1.0)
        w (get (:w (get (:jobmix model) (:type j))) (:name m))]
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
;  (when (:status (lookup model m-name)) ; diag
;    (throw (ex-info "Machine already busy!" {:machine m-name :model model})))
  (let [b-name (takes-from model m-name)
        b (lookup model b-name)
        job (-> (first (:holding b))
                (assoc :starts (:clock model)))
        [ends future] (job-updates! model job (lookup model m-name))
        job (assoc job :ends ends)]
;    (when (= :up (first future))
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
      `((~(:clock model) (add-job ~(new-job model) ~(:entry-point model)))))))

(defn bring-machine-up?
  "Return an action to bring a down machine up."
  [model]
  (let [bring-up (filter #(down? %) (machines model))]
    (when (not-empty bring-up)
      (map (fn [mn] `(~(second (:future mn)) (bring-machine-up ~(:name mn)))) ; second of two places time advances...
           bring-up))))                                                      ; ...it is seldom used.

(defn new-blocked?
  "Return record-actions to list newly blocked machines."
  [model]
  (let [clock (:clock model)
        old-blocked (:blocked model)
        new-blocked  (filter #(and 
                               (buffer-full? model %)
                               (not (contains? old-blocked (:name %)))
                               (occupied? %) 
                               (finished? model %)) 
                             (machines model))]
    (when (not-empty new-blocked)
      (map (fn [mn] `(~clock (new-blocked ~(:name mn)))) new-blocked))))

(defn new-unblocked?
  "Return record-actions to unlist certain machines as blocked."
  [model]
  (let [clock (:clock model)
        old-blocked (:blocked model)
        new-unblocked (filter #(and
                                (contains? old-blocked (:name %))
                                (not (buffer-full? model %)))
                              (machines model))]
    (when (not-empty new-unblocked)
      (map (fn [mn] `(~clock (new-unblocked ~(:name mn)))) new-unblocked))))

(defn new-starved?
  "Return record-actions to add starved machines."
  [model]
  (let [clock (:clock model)
        old-starved (:starved model)
        new-starved (filter #(and 
                              (finished? model %)
                              (not (contains? old-starved (:name %)))
                              (feed-buffer-empty? model %))
                            (machines model))]
    (when (not-empty new-starved)
      (map (fn [mn] `(~clock (new-starved ~(:name mn)))) new-starved))))

(defn new-unstarved?
  "Return record-actions to unlist certain machines as starved."
  [model]
  (let [clock (:clock model)
        old-starved (:starved model)
        new-unstarved (filter #(and
                                (contains? old-starved (:name %))
                                (not (feed-buffer-empty? model %)))
                              (machines model))]
    (when (not-empty new-unstarved)
      (map (fn [mn] `(~clock (new-unstarved ~(:name mn)))) new-unstarved))))

(defn advance2buffer?
  "Return actions to buffer and times at which these can occur (which may be later than clock)."
  [model]
  (let [advance (filter #(and
                          (occupied? %) ; maybe not finished yet; will be here. 
                          (not (buffer-full? model %)))
                        (machines model))]
    (when (not-empty advance)
      (map (fn [mn] `(~(max (:clock model) (:ends (:status mn))) ; Key idea!
                      (advance2buffer ~(:name mn)))) advance))))

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
      (map (fn [mn] `(~clock (advance2machine ~(:name mn)))) advance))))

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
      (let [min-time (apply min (map first all))]
        (filter (fn [[time _]] (= time min-time)) all)))))
      
(defn run-action
  "Run a single action, returning the model."
  [model [fname & args]]
  ;(reset! +diag+ {:where 'run-action :fname fname :args args :model model})
  (apply (partial (var-get (resolve fname)) model) args))

(defn run-actions
  "Run a list of actions in the order they appear in actions; 
   update the model while doing so."
  [model actions]
  (reduce (fn [m a] (run-action m (vec a))) model actions)) 

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

(defn preprocess-equip ; POD defmethod
  [[k e]]
  (cond (= (type e) ExpoMachine)
        (as-> e ?m 
          (assoc ?m :mchain (exponential-up&down (:lambda ?m) (:mu ?m)))
          (assoc ?m :future ((:mchain ?m)))
          (assoc ?m :name k)
          (dissoc ?m :lambda)
          (dissoc ?m :mu)
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

(defn now []
  (.format
   (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
   (new java.util.Date)))

(defn avg [col]
  (if (not-empty col)
    (/ (apply + col) (count col))
    :na))

(defn variance [col mean]
  (avg (map #(* (- mean %) (- mean %)) col)))

(defn results-form 
  "Create a results for map."
  [model]
   (as-> {:residence-sum 0.0, :njobs 0} ?r
     (into ?r (map (fn [m] [m {:blocked 0.0 :starved 0.0 :bs nil :ss nil}])
                   (:machines model)))
     (into ?r (map (fn [b] [(:name b) (assoc (zipmap (range (inc (:N b))) (repeat (+ 2 (:N b)) 0.0))
                                             :lastclk (:warm-up-time (:params model)))])
                   (map #(lookup model %) (:buffers model))))))

(defn calc-basics
  "Update results with percent time blocked, and job residence time."
  [res model]
  (let [warm-up-time (:warm-up-time (:params model))
        run-time (- (:run-to-time (:params model)) warm-up-time)
        residence-sum (:residence-sum res)
        njobs (:njobs res)]
    {:runtime run-time
     :TP (float (/ njobs run-time))
     :observed-residence-time (/ residence-sum njobs)
     :number-of-jobs njobs
     :blocked (into {} (map #(vector % (/ (:blocked (% res)) run-time)) (:machines model)))
     :starved (into {} (map #(vector % (/ (:starved (% res)) run-time)) (:machines model)))
     :wip (into {} (map (fn [bf] (vector bf (/ (apply + (map (fn [n t] (* n t))
                                                             (keys (dissoc (bf res) :lastclk))
                                                             (vals (dissoc (bf res) :lastclk))))
                                               run-time)))
                        (:buffers model)))}))

(defn calc-bneck
  "Update results with identification of bottlenecks."
  [res model]
  (let [m-order (filter #(machine? (lookup model %)) (:topology model))]
    (letfn [(mIndex [m] (inc (.indexOf m-order m)))
            (bl [i] (nth (vals (:blocked res)) (dec i)))
            (st [i] (nth (vals (:starved res)) (dec i)))]
      (let [candidates (filter #(when (not= % (last m-order))
                                  (< (bl (mIndex %)) (st (inc (mIndex %)))))  m-order)]
        (if (= 1 (count candidates))
          (assoc res :bneck (first candidates))
          (let [m  (count (:machines model))
                severity (map (fn [i]
                                (cond
                                  (= i 1) (abs (- (bl 1) (st 2)))
                                  (= i m) (abs (- (bl (dec m)) (st m)))
                                  :else   (+ (abs (- (bl (dec i)) (st i)))
                                             (abs (- (bl i) (st (inc i)))))))
                              (range 2 m))]
            (assoc res :bneck 
                   (nth m-order (+ (.indexOf (max severity) severity) 2)))))))))

(defn calc-residence-time ; POD update for MJP
  "Calculate job residence time as function of WIP, SL and BL."
  [res model]
  (let [job (map->Job {:type :jobType1})
        j-requires (map #(job-requires model job %) (map #(lookup model %) (:machines model)))
        j-requires (map (fn [jr mn] (* (+ 1.0 (mn (:starved res)) (mn (:blocked res))) jr))
                        j-requires (:machines model))
        j-requires (map (fn [jr bflen] (+ jr (* bflen jr)))
                        j-requires (conj (vals (:wip res)) 0.0))]
    (assoc res :computed-residence-time (apply + j-requires))))

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

(def +log-results+ (atom nil))

(defn log [log-map]
  (when (> (:clk log-map) @+warm-up-time+)
    (swap! +log-results+
           #(case (:act log-map)
              :bj (buf+    % log-map)
              :sm (buf-    % log-map)
              :ej (end-job % log-map)
              :bl (block+  % log-map) 
              :ub (block-  % log-map)
              :st (starve+ % log-map)
              :us (starve- % log-map)))))

(defn compare-results
  [res given]
  (letfn [(div0 [n d]
            (if (= 0.0 d) :na (/ n d)))]
    (-> res
        (assoc :compared-block
               (into (sorted-map) (map (fn [[k r]] [k (div0 r (k (:blocked given)))]) (:blocked res))))
        (assoc :compared-starve
               (into (sorted-map) (map (fn [[k r]] [k (div0 r (k (:starved given)))]) (:starved res)))))))

(def diag-fig32a
  {:blocked
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
    :m5 0.1860},
   :status :normal-end})

(defn analyze-results [model]
  "Read the w-<date> output file and compute results. model argument is only for (:machines model)."
    (as-> @+log-results+ ?r
      (calc-basics ?r model)
      (calc-bneck ?r model)
      (calc-residence-time ?r model)
      #_(compare-results ?r diag-fig32a)))

(defn clear-memos
  []
  (m/memo-clear! buffers-to)
  (m/memo-clear! feeds)
  (m/memo-clear! takes-from))

(defn main-loop 
  "Run a simulation."
  [model]
  ;;(reset! +diag-expect-job+ 1)
  (clear-memos)
  (let [model (preprocess-model model)]
    (reset! +log-results+ (results-form model))
    (as-> model ?m
      (loop [model ?m]
        (if-let [run (not-empty (runables model))]
          (let [model (advance-clock model (first (first run)))]
            (if true 
              (if (or (and (:run-to-job (:params model))
                           (<= (:current-job (:params model)) (:run-to-job (:params model))))
                      (and (:run-to-time (:params model))
                           (<= (:clock model) (:run-to-time (:params model)))))
                (recur (run-actions model (map second run)))
                (assoc-in model [:params :status] :normal-end))
              (assoc-in model [:params :status] :over-ran)))
          (assoc-in model [:params :status] :no-runables)))
      (reset! +diag+ ?m)
      (analyze-results ?m) 
      (assoc ?m :status (:status (:params model))))))

(def f1
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
    :entry-point :m1 ; 
    :params {:warm-up-time 2000 :run-to-time 20000}  
    :jobmix {:jobType1 (map->JobType {:portion 1.0
                                      :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})}}))

(def f2
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
    :jobmix {:jobType1
             (map->JobType {:portion 1.0
                                      :w {:m1 4.0, :m2 4.0, :m3 4.0, :m4 4.0, :m5 4.0}})}}))
#_(defn check-m1 []
  (let [len (count ws-final)]
    (loop [sum 0.0
           cnt 0]
      (if (< cnt len)
        (recur (+ sum (- (:clk (nth ws-final (inc cnt)))
                         (:clk (nth ws-final cnt))))
               (+ cnt 2))
        (/ sum 18000)))))







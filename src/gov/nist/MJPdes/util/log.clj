(ns gov.nist.MJPdes.util.log
  "Data collection, editing, and printing of DES events"
  (:require
   [clojure.pprint :refer (cl-format pprint pp)]
   [clojure.spec.alpha :as s]
   [gov.nist.MJPdes.util.utils :as util :refer (ppp ppprint)]))

(def ^:dynamic *log-steady* "Collects essential data for steady-state calculations." nil)
(def ^:private diag (atom nil))

(defn detail
  "Return a map of operation details including what jobs are in what machines or buffers."
  [model]
  (when (-> model :report :job-detail?)
    (let [bufs (:buffers model)
          machines (:machines model)]
      {:run
       (zipmap
        machines
        (map (fn [mach-key]
               (-> model :line mach-key :status :id))
             machines))
       :bufs
       (zipmap
        bufs
        (map (fn [buf-key]
               (mapv :id (-> model :line buf-key :holding)))
             bufs))})))

(defn log
  "Add to the log-buffer. On a clock tick it will be cleaned and written to log."
  [model msg-map]
  (let [keep? (or (-> model :report :up&down?)
                  (not (#{:up :down} (:act msg-map))))]
    (cond-> model
      keep? (update :log-buf #(conj % msg-map)))))

(defn clean-log-buf
  "Remove acts that reverse themselves (i.e. :bl/:ub :st/us) in the argument collection.
   N.B. Assumes a lot about the input: (1) all acts have same :time, (2) no more than one 
   :bl, :st, :ub, and :us allowed per machine in the argument."
  [acts]
  (as-> acts ?a
       (group-by :m ?a)
       (mapcat (fn [[m acts]]
                 (->> acts
                      (group-by #(cond (#{:bl :ub} (:act %)) :bl-ub
                                       (#{:st :us} (:act %)) :st-us
                                       :else :other))
                      (reduce (fn [accum [k v]]
                                (if (and (== (count v) 2)
                                         (or (= k :bl-ub) (= k :st-us)))
                                  accum
                                  (conj accum v)))
                              [])))
               ?a)
       (reduce #(into %1 %2) [] ?a)))

(s/def ::act keyword?)
(s/def ::clk float?)
(s/def ::msg (s/keys :req-un [::clk ::act]))
(s/def ::buf (s/coll-of ::msg))
;;; Call it with a collection of messages all happening at the same time.  
(s/fdef clean-log-buf :args (s/and (s/cat :buf ::buf)
                                   #(let [clk (-> % :args :buf first :clk)]
                                      (every? (fn [msg] (== clk (:clk %)))
                                              (-> % :args :buf)))))


(declare buf+ buf- end-job block+ block- starve+ starve-)
(defn add-compute-log! ; CIDER debugger has trouble with this!
  "Collect data essential for calculating performance measures."
  [msg]
  (dosync
   (alter *log-steady*
          #(case (:act msg)
             :bj (buf+    % msg)
             :sm (buf-    % msg)
             :ej (end-job % msg)
             :bl (block+  % msg) 
             :ub (block-  % msg)
             :st (starve+ % msg)
             :us (starve- % msg)
             :aj @*log-steady*
             :up @*log-steady*
             :down @*log-steady*))))

(declare print-now? pretty-buf print-lines)
(defn push-log
  "Clean up the :log-buf and record (add-compute-log!) all msgs accumulated in it
   since the last clock tick. The :log-buf has msgs from the next clock tick,
   so you have to sort them and only push the old ones."
  [model]
  (let [buf (:log-buf model)]
    (when (not-empty buf)
      (let [min-time (reduce #(min %1 (:clk %2)) ##Inf buf)
            max-time (reduce #(max %1 (:clk %2)) 0 buf)]
        (if (> max-time min-time) ; Time to log!
          (let [parts (group-by #(if (== (:clk %) min-time) :now :later) buf)
                clean-buf (cond->> (:now parts)
                            (not (:job-detail? model))  (mapv #(dissoc % :dets)),
                            (not-empty (:now parts))    (clean-log-buf))
                warm-up (-> model :params :warm-up-time)]
            (when (>= min-time warm-up)
              (run! add-compute-log! clean-buf))
            (cond-> model
              (print-now? model clean-buf min-time) (print-lines clean-buf)
              true (assoc :log-buf (-> parts :later vec))))
          model)))))

(defn print-lines
  "pretty-print the lines, updating (-> model :report :line-cnt)."
  [model clean-buf]
  (let [fmt (str "{:clk" (-> model :params :time-format) " :act " "~18A" "~{ ~A~}}~%")
        buf (pretty-buf model clean-buf)
        line-num (ref (-> model :report :line-cnt))]
    (when-not (-> model :report :continuous?)
      (run! (fn [line]
              (cl-format *out* fmt
                         (:clk line)
                         (:act line)
                         (-> (dissoc line :clk :act)
                             (assoc :line @line-num)
                             vec
                             flatten))
              (dosync (alter line-num inc)))
            buf))
    (cond-> model
      (-> model :report :continuous?)
      (update :print-buf
              (fn [pb] (into pb (map #(assoc %1 :line %2)
                                     buf
                                     (range @line-num (+ @line-num (count buf))))))), 
      (-> model :report :diag-log-buf?)
      (update :diag-log-buf #(into % buf)),
      true
      (update-in [:report :line-cnt] #(+ % (count buf))))))

;;;{:residence-sum 0.0,
;;; :njobs 0,
;;; :m1 {:blocked 0.0, :starved 0.0, :bs nil, :ss nil},
;;; :m2 {:blocked 0.0, :starved 0.0, :bs nil, :ss nil},
;;; :b1 {0 0.0, 1 0.0, 2 0.0, 3 0.0, :lastclk 2000}}
(defn steady-form
  "Create a results map."
  [model]
  (as-> {:residence-sum 0.0, :njobs 0} ?r
    (into ?r (map (fn [m] [m {:blocked 0.0 :starved 0.0 :bs nil :ss nil}])
                  (:machines model)))
    (into ?r (map (fn [b] [(:name b) (assoc (zipmap (range (inc (:N b))) (repeat (+ 2 (:N b)) 0.0))
                                            :lastclk (-> model :params :warm-up-time))])
                  (map #(-> model :line %) (:buffers model))))))

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
  (assoc-in r [(:m o) :bs] (:clk o)))

(s/def ::njobs (s/and integer? #(>= % 0)))
(s/def ::residence-sum (s/and number? #(>= % 0)))
(s/def ::steady (s/keys :req-un [::residence-sum ::njobs]))
(s/fdef block+
           :args (s/cat :steady ::steady :msg ::msg)
           :fn (fn [r o] (not (:bs ((:m o) r))))) ; not blocking twice 

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
  (assoc-in r [(:m o) :ss] (:clk o)))

(s/fdef starve+
           :args (s/cat :steady ::steady :msg ::msg)
           :fn (fn [r o] (not (:ss ((:m o) r)))))

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

;;;============= Pretty printing ===================================================
(defn print-now?
  "Returns true if it is time to print log entries."
  [model clean-buf now]
  (and (-> model :report :log?)
       (not-empty clean-buf)
       (> (-> model :report :max-lines) (-> model :report :line-cnt))
       (>= now (-> model :params :warm-up-time))))

(defn shorten-msg-floats
  [model msg]
  (let [short-fn #(read-string (cl-format nil (-> model :params :time-format) %))]
    (cond-> msg
      (contains? msg :clk) (update :clk short-fn)
      (contains? msg :ent) (update :ent short-fn)
      (contains? msg :ends) (update :ends short-fn))))

(defn implies-machine
  "Returns machine referenced/implied in messages that don't specify a machine. 
   If a buffer n is references, machine n+1 is pulling from it.
   Returns nil if msg contains neither :bf or :m"
  [model msg]
  (let [act (:act msg)
        buf (:bf msg)]
    (cond (contains? msg :m) (:m msg),
          (= act :aj) (:entry-point model),
          (= act :bj) (some #(when (= buf (util/buffers-to model %)) %) (:machines model)),
          (= act :sm) (some #(when (= buf (util/takes-from model %)) %) (:machines model)))))

(defn mjp2pretty-name
  "Return a prettyfied (relative to the usual 2 character keyword) :act for the msg."
  [msg]
  (let [m (:m msg)]
    (cond (= :aj (:act msg)) (read-string (cl-format nil "~A-start-job"    m)),
          (= :ej (:act msg)) (read-string (cl-format nil "~A-complete-job" m)),
          (= :sm (:act msg)) (read-string (cl-format nil "~A-start-job"    m)),
          (= :bj (:act msg)) (read-string (cl-format nil "~A-complete-job" m)),
          (= :bl (:act msg)) (read-string (cl-format nil "~A-blocked"      m)),
          (= :ub (:act msg)) (read-string (cl-format nil "~A-unblocked"    m)),
          (= :st (:act msg)) (read-string (cl-format nil "~A-starved"      m)),
          (= :us (:act msg)) (read-string (cl-format nil "~A-unstarved"    m)),
          (= :up (:act msg)) (read-string (cl-format nil "~A-up"           m)),
          (= :down (:act msg)) (read-string (cl-format nil "~A-down"       m)))))

(defn prettyfy-msg
  "Interpret/translate the SCADA log. (Give pretty-fied pn names to MJPdes output.)" 
  [model msg]
  (as-> msg ?msg
    (assoc ?msg :mjpact (:act ?msg))
    (assoc ?msg :act (mjp2pretty-name ?msg))))

;;; POD NOTE! If you fail to include a key here, the message printing gets
;;;     messed up, with values not matched to the correct keys!
(def msg-key-order "Order we want message keys to appear in printed logs"
  [:clk :act :m :mjpact :bf :jt :n :ent :ends :j :dets :line])

(defn msg-key-compare
  "Return true if k1 is before k2 in the sort order msg-key-order."
  [k1 k2]
  (let [keys ^clojure.lang.PersistentVector msg-key-order]
    (if (and (some #(= k1 %) msg-key-order)
             (some #(= k2 %) msg-key-order))
      (< (.indexOf keys k1)
         (.indexOf keys k2))
      false)))

(defn pretty-buf
  "Reorder and translate items in buffer, truncate floats"
  [model clean-buf]
  (->> clean-buf
       (map #(prettyfy-msg model %))
       (map  #(shorten-msg-floats model %))
       (map  #(into (sorted-map-by msg-key-compare) %))))

(defn pretty-model
  "Remove a few things from model to make it print cleaner."
  [model]
  (update-in model [:line]
             (fn [line]
               (reduce (fn [l equip-name]
                         (update-in l [equip-name]
                                    #(-> %
                                         (dissoc :name)
                                         (dissoc :status)
                                         (dissoc :mchain)
                                         (dissoc :up&down)
                                         (dissoc :holding))))
                       line
                       (keys line)))))

(defn output-sims
  "Print the simulation results to *out*. If there is just one sim, return it.
   This is intended to be the return value of core/main-loop."
  [model sims]
  (doall (map deref sims))
  (if (or (not (contains? model :number-of-simulations))
          (== 1 (:number-of-simulations model)))
    (do
      (print "#_")
      (-> sims first deref (dissoc :jobmix) pprint)
      (-> sims first deref))
    (map (fn [sim]
           (print "#_")
           (pprint (dissoc @sim :jobmix)))
         sims)))

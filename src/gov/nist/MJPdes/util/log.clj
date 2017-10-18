(ns gov.nist.MJPdes.util.log
  "Data collection, editing, and printing of DES events"
  (:require
   [clojure.spec.alpha :as spec]
   [clojure.pprint :refer (cl-format pprint pp)]
   [gov.nist.MJPdes.util.utils :as util]))

(def ^:dynamic *log-steady* "Collects essential data for steady-state calculations." nil)

(defn log
  "Add to the log-buffer. On a clock tick it will be cleaned and written to log."
  [model log-map]
  (update model :log-buf #(conj % log-map)))

(defn clean-log-buf
  "Return a vector of log msgs with superfluous block/unblock starve/unstarve removed.
   Such msgs are superfluous if they happen at the same time."
  [buf]
  (let [rem-fn (fn [acts ms log]
                 (reduce
                  (fn [log m]
                    (reduce (fn [log act] (remove #(and (= (:m %) m) (= (:act %) act)) log))
                            log acts))
                  log ms))
        bms (distinct (map :m (filter #(= (:act %) :bl) buf)))
        sms (distinct (map :m (filter #(= (:act %) :st) buf)))]
    (as-> buf ?log
      (if-let [bm (map :m (filter (fn [msg] (and (= (:act msg) :ub)
                                                 (some #(= (:m msg) %) bms)))
                                  ?log))]
        (rem-fn [:bl :ub] bm ?log)
        ?log)
      (if-let [sm (map :m (filter (fn [msg] (and (= (:act msg) :us)
                                                 (some #(= (:m msg) %) sms)))
                                  ?log))]
        (rem-fn [:st :us] sm ?log)
        ?log))))

;;; POD This will need some work as lines get more sophisticated. 
(defn upstream?
  "Returns true if equip1 is upstream of equip2"
  [model equip1 equip2]
  (let [top ^clojure.lang.PersistentVector (:topology model)]
    (and (some #(= % equip1) top)
         (some #(= % equip2) top)
         (> (.indexOf top equip1)
            (.indexOf top equip2)))))

(defn upstream-msg?
  "Returns true if msg1 refers to equipment upstream of msg2"
  [model msg1 msg2]
  (let [equip1 (or (:m msg1) (:bf msg1))
        equip2 (or (:m msg2) (:bf msg2))]
    (if (and equip1 equip2)
      (upstream? model equip1 equip2)
      false)))

(spec/def ::act keyword?)
(spec/def ::clk float?)
(spec/def ::msg (spec/keys :req-un [::clk ::act]))
(spec/def ::buf (spec/coll-of ::msg))
;;; Call it with a collection of messages all happening at the same time.  
(spec/fdef clean-log-buf :args (spec/and (spec/cat :buf ::buf)
                                         #(let [clk (-> % :args :buf first :clk)]
                                            (every? (fn [msg] (== clk (:clk %)))
                                                    (-> % :args :buf)))))


(declare buf+ buf- end-job block+ block- starve+ starve-)
(defn add-compute-log!
  "Collect data essential for calculating performance measures."
  [msg]
  (swap! *log-steady*
         #(case (:act msg)
            :bj (buf+    % msg)
            :sm (buf-    % msg)
            :ej (end-job % msg)
            :bl (block+  % msg) 
            :ub (block-  % msg)
            :st (starve+ % msg)
            :us (starve- % msg)
            :aj @*log-steady*)))

(declare print-now? pretty-buf)
(defn push-log
  "Clean up the :log-buf and record (add-compute-log!) all msgs accumulated in it
   since the last clock tick. The :log-buf has msgs from the next clock tick,
   so you have to sort them and only push the old ones."
  [model up-to-clk]
  ;(doall (map println (-> model :log-buf)))
  ;(assoc model :log-buf []) ; diag -- these two instead of clean-log-buf. 
  (let [parts {:now   (filter #(< (:clk %) up-to-clk) (-> model :log-buf))
               :later (remove #(< (:clk %) up-to-clk) (-> model :log-buf))}
        clean-buf (clean-log-buf (:now parts))
        cnt (atom 0)
        warm-up (-> model :params :warm-up-time)]
    (when (> up-to-clk warm-up)
      (doall (map #(when (> (:clk %) warm-up)
                     (add-compute-log! %))
                  clean-buf)))
    (when (print-now? model parts up-to-clk)
      (reset! cnt (count clean-buf))
      (let [fmt (str "{:clk" (-> model :params :time-format) "~{ ~A~}}~%")]
        (doall (map #(cl-format *out* fmt
                                (:clk %)
                                (-> (dissoc % :clk) vec flatten))
                    (pretty-buf model clean-buf))))) ; print, possibly to log file.
    (as-> model ?m
      (if (and (-> model :report :diag-log-buf?)
               (print-now? model parts up-to-clk))
        (update-in ?m [:diag-log-buf] #(into % clean-buf))
        ?m)
      (assoc ?m :log-buf (vec (:later parts)))
      (update-in ?m [:report :line-cnt] #(+ % @cnt)))))


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
                  (map #(util/lookup model %) (:buffers model))))))

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

(spec/def ::njobs (spec/and integer? #(>= % 0)))
(spec/def ::residence-sum (spec/and number? #(>= % 0)))
(spec/def ::steady (spec/keys :req-un [::residence-sum ::njobs]))
(spec/fdef block+
           :args (spec/cat :steady ::steady :msg ::msg)
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

(spec/fdef starve+
           :args (spec/cat :steady ::steady :msg ::msg)
           :fn (fn [r o] (not (:ss ((:m o) r))))) ; not starving twice 


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
  "Returns true if it is time to add to the log."
  [model parts up-to-clk]
  (and (-> model :report :log?)
       (not-empty (:now parts))
       (> (-> model :report :max-lines) (-> model :report :line-cnt))
       (> up-to-clk (-> model :params :warm-up-time))))

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
          (= :ej (:act msg)) (read-string (cl-format nil "~A-move-off"     m)),
          (= :sm (:act msg)) (read-string (cl-format nil "~A-start-job"    m)),
          (= :bj (:act msg)) (read-string (cl-format nil "~A-move-off"     m)),
          (= :bl (:act msg)) (read-string (cl-format nil "~A-blocked"      m)),
          (= :ub (:act msg)) (read-string (cl-format nil "~A-unblocked"    m)),
          (= :st (:act msg)) (read-string (cl-format nil "~A-starved"      m)),
          (= :us (:act msg)) (read-string (cl-format nil "~A-unstarved"    m)))))

(defn prettyfy-msg
  "Interpret/translate the SCADA log. (Give pretty-fied pn names to MJPdes output.)" 
  [model msg]
  (let [m (implies-machine model msg)]
    (as-> msg ?msg
        (assoc ?msg :m m)
        (assoc ?msg :act (mjp2pretty-name ?msg)))))

(def msg-key-order "Order we want message keys to appear in printed logs"
  [:clk :act :jt :m :bf :n :ent :ends :j])

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
       (sort #(upstream-msg? model %1 %2)) ; POD needs work
       (map  #(shorten-msg-floats model %))
       (map  #(into (sorted-map-by msg-key-compare) %))))

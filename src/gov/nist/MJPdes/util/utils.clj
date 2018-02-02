(ns gov.nist.MJPdes.util.utils
  (:require [clojure.pprint :refer (cl-format pprint pp)]
            [clojure.spec.alpha :as s]))

;;; Note: I (try to)  use 'm' or 'm-name' for the name of a machine and 'mach' for its map. 

(alias 'core 'gov.nist.MJPdes.core)

;;;=== General =========================
(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140
           #_ #_ *print-length* 10]
    (pprint *1)))

(defn ppprint
  ([arg]
   (binding [clojure.pprint/*print-right-margin* 140
             #_ #_ *print-length* 10]
     (pprint arg)))
  ([arg len]
    (binding [clojure.pprint/*print-right-margin* 140
              *print-length* len]
      (pprint arg))))

(defn break
  ([] (throw (ex-info "Break!" {})))
  ([text] (throw (ex-info text {})))
  ([text args] (throw (ex-info text args))))

(defn mean
  [v]
  (/ (apply + v) (count v)))

(defn variance
  " Var(X) = E[(X - mu)^2]"
  [v]
  (let [avg (mean v)]
    (mean (vec (map (fn [x]
                      (let [dif (- x avg)]
                        (* dif dif)))
                    v)))))

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

(defn up? [mach]
  (s/assert ::core/ExpoMachine mach)
  (= :down (second (:future mach))))

(defn down? [mach]
  (s/assert ::core/ExpoMachine mach)    
  (= :up (second (:future mach))))

(defn finished? 
  "Returns true if the machine (arg is map) either does not have a job
   or has one but is blocked."
  [model mach]
  (let [status (:status mach)]
    (or (not status)
        (>= (:clock model) (:ends status)))))

(defn occupied?
  "Returns true if machine (arg is map) has a a job (running or blocked)."
  [mach]
  (s/assert ::core/machine mach)
  (:status mach))

(defn feed-buffer-empty? 
  "Returns true if buffer feeding machine (arg is map) is empty."
  [model mach] 
  (when (not= (:name mach) (:entry-point model))
    (let [buf (get (:line model) (takes-from model (:name mach)))]
      (== (count (:holding buf)) 0))))

(defn buffer-full? 
  "Returns true if the buffer that machine (arg is map) places completed work on is full."
  [model mach] 
  (when-let [buf (get (:line model) (buffers-to model (:name mach)))] ; last machine cannot be blocked.
    (== (count (:holding buf)) (:N buf))))

(defn job-requires
  "Total time that job j requires on a machine m (arg is name), (w_{ij}/W_i)"
  [model j m]
  (let [mach (-> model :line m)
        W (or (:W mach) 1.0)
        w (get (:w (get (:jobmix model) (:type j))) m)] 
    (/ w W)))

(defn next-machine?
  "Returns true if the next machine down the line from m1 is m2."
  [model m1 m2]
  (let [buf1 (buffers-to model m1)
        buf2 (takes-from model m2)]
    (and buf1 (= buf1 buf2))))

(defn prev-machine?
  "Returns true if the previous machine up the line from m1 is m2"
  [model m1 m2]
  (let [buf1 (takes-from model m1)
        buf2 (buffers-to model m2)]
    (and buf1 (= buf1 buf2))))

(defn upstream?
  "Returns true if equip1 is upstream of equip2"
  [model equip1 equip2]
  (let [top ^clojure.lang.PersistentVector (:topology model)]
    (and (not= equip1 equip2)
         (some #(= % equip1) top)
         (some #(= % equip2) top)
         (< (.indexOf top equip1)
            (.indexOf top equip2)))))





  

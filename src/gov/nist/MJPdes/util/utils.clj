(ns gov.nist.MJPdes.util.utils
  (:require [clojure.pprint :refer (cl-format pprint pp)]
            [clojure.spec.alpha :as s]))

(alias 'core 'gov.nist.MJPdes.core)

;;;=== General =========================
(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140
            *print-length* 10]
    (pprint *1)))

(defn ppprint
  ([arg]
   (binding [clojure.pprint/*print-right-margin* 140
             *print-length* 10]
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

#_(defn analyze-results [filename]
  "Read an output file and perform various calculations."
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (let [results (loop [mp (edn/read {:eof :eof} in)
                         res {:starve []}]
                    (if (not= :eof mp)
                      (recur (edn/read {:eof :eof} in)
                             (update-in res [:starve] conj (:m2 (:starved mp))))
                      res))]
      ;; Starvation
      (let [starve (:starve results)]
        {:starvation {:min (apply min starve)
                      :max (apply max starve)
                      :mean (mean starve)
                      :variance (variance starve)
                      :values starve}}))))

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
  (s/assert ::core/ExpoMachine m)
  (= :down (second (:future m))))

(defn down? [m]
  (s/assert ::core/ExpoMachine m)    
  (= :up (second (:future m))))

(defn finished? [model m]
  "Returns true if the machine is finished with what it is working on???"
  (let [status (:status m)]
    (or (not status)
        (>= (:clock model) (:ends status)))))

(defn occupied? [m]
  (s/assert ::core/machine m)
  (:status m))

(defn feed-buffer-empty? [model m] 
  "Returns true if buffer feeding machine m is empty." 
  (when (not= (:name m) (:entry-point model))
    (let [buf (get (:line model) (takes-from model (:name m)))]
      (== (count (:holding buf)) 0))))

(defn buffer-full? [model m] 
  "Returns true if the buffer that machine m places completed work on is full."
  (when-let [buf (get (:line model) (buffers-to model (:name m)))] ; last machine cannot be blocked.
    (== (count (:holding buf)) (:N buf))))

(defn job-requires
  "Total time that job j requires on a machine m, (w_{ij}/W_i)"
  [model j m]
  (let [mach (-> model :line m)
        W (or (:W mach) 1.0)
        w (get (:w (get (:jobmix model) (:type j))) m)] 
    (/ w W)))

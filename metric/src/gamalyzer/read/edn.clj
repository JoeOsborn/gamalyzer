(ns gamalyzer.read.edn
  (:require [clojure.edn :as edn]
            [multiset.core :as ms]
						[clojure.java.io :refer [file]]
						[gamalyzer.data.input :refer [make-input make-domains make-traces make-trace expand-domain player]])
  (:import (java.io PushbackReader FileReader)))

(defn- open-log [path blacklist]
  (let [file (FileReader. path)
        pb (PushbackReader. file)]
		{:file file :reader pb :blacklist blacklist}))

(defn- close-log [h]
  (.close (:file h))
  nil)

(defn- read-log-entry [h] (edn/read {:eof nil} (:reader h)))

; [time player (prochead args)|procname (lev choiceid) choice inputs] ->
; [time player determinant values]
; [([procname |args|] lev-choiceid choice) (inputs* player args*)]
(defn- input-parts [i]
  (let [t (nth i 0)
        player (nth i 1)
        ph (nth i 2)
        [name arity args]
        (if (keyword? ph)
          [ph 0 '()]
          [(first ph) (- (count ph) 1) (rest ph)])
        lev-choiceid (nth i 3)
        choice (nth i 4)
        inputs (nth i 5)
        k (list [name arity] lev-choiceid choice)
        v (concat inputs (list player) args)]
    [t player k v]))

(defn- read-log-trace [h domains]
  (if-let [start (read-log-entry h)]
    (let [fs (first start)
          ss (second start)
          bl (:blacklist h)]
      (if (= fs :log_start)
        ; [:i :system :root (:game 0) :player_count (2 4 2)] ....
        ; until :log_end
        ; read the whole thing into a ... vector? cons list?
        ; and then return it.
        (loop [v (transient (vector))
               doms domains]
          (let [t (read-log-entry h)]
            (cond
             (= t :log_end) (make-traces [(make-trace ss (persistent! v))] doms)
             (= (first t) :log_start)
               (do
                 (close-log h)
                 (throw (Exception. "New log started in the middle of existing log")))
             (= :i (first t))
               (let [t' (case (count t)
                          6 (concat [(count v)] (rest t))
                          7 (rest t))
                     inp (apply make-input (input-parts t'))]
                 (cond
                  (contains? bl (player inp))
                    (recur v doms)
                  true
                    (let [new-domains (expand-domain inp doms)]
                      (recur (conj! v inp) new-domains))))
             true
               (do
								 (println "Skipping unhandled or out of scope term " t)
								 (recur v doms)))))
        (do
					(println "Skipping unhandled or out-of-scope top-level term " start)
					(recur h domains))))
    nil))

(defn read-logs
	([paths blacklist indoms]
	 (reduce (fn [{traces :traces
								 doms :domains} file]
						 (let [{new-traces :traces
										new-doms :domains}
									 (read-logs file :all blacklist doms)]
							 (make-traces (into traces new-traces) new-doms)))
					 (make-traces [] (or indoms (make-domains)))
					 paths))
  ([path how-many blacklist indoms]
   (let [log (open-log path blacklist)
         domains (if (nil? indoms) (make-domains) indoms)]
     (loop [ts (transient (vector))
            remaining how-many
            doms domains]
       (if (or (= remaining :all) (> remaining 0))
         (if-let [{[trace] :traces, new-doms :domains} (read-log-trace log doms)]
           (recur
            (conj! ts trace)
            (if (number? remaining) (- remaining 1) remaining)
            new-doms)
           (do
             (close-log log)
             (make-traces (persistent! ts) doms)))
         (do
           (close-log log)
           (make-traces (persistent! ts) doms)))))))

(defn sample-data
  ([] (sample-data nil))
  ([lev]
   (let [files (filter #(.endsWith (.getName %)
                                   (str (if (nil? lev) "" (str "_" lev)) ".trace"))
                       (file-seq (file "resources/traces/")))]
		 (read-logs files #{} nil))))

(defn read-path [matching-files real-path excess-path settings]
	(read-logs matching-files #{} (make-domains)))

; Informal tests and usage examples.

#_(:traces (time (read-logs "/Users/jcosborn/Projects/gamalyzer/vis/resources/traces/refraction/5/refraction.5.i.trace" 2 (hash-set) nil)))

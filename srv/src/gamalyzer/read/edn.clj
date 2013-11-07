(ns gamalyzer.read.edn
	(:require [clojure.edn :refer [read]]
						[multiset.core :as ms]
						[gamalyzer.data.input :refer [make-input make-domains expand-domain]])
	(:import (java.io PushbackReader FileReader)))

(defn open-log [path blacklist]
	(let [file (FileReader. path)
		    pb (PushbackReader. file)
		    props (read pb)]
		(if (= (first props) :properties)
		; later: do stuff with (second props) i.e. prop-list, which is a pairlist like:
		; ([:game :dominion] [:root :root] [:events (:i)])
			{:file file :reader pb :blacklist blacklist}
			(do
				(. file close)
				(throw (Exception. "no log properties"))))))

(defn close-log [h]
	(. (:file h) close)
	nil)

(defn read-log-entry [h] (read {:eof nil} (:reader h)))

; [:i player (prochead args)|procname (lev choiceid) choice inputs] ->
; [determinant values]
; [([procname |args|] lev-choiceid choice) (inputs* player args*)]
(defn input-parts [i]
	(let [player (nth i 1)
				ph (nth i 2)
				[name arity args]
					(if (keyword? ph)
						[ph 0 '()]
						[(first ph) (- (count ph) 1) (rest ph)])
				lev-choiceid (nth i 3)
				choice (nth i 4)
				inputs (nth i 5)
				k (list [name arity] lev-choiceid choice)
				v (concat inputs '(player) args)]
		[k v]))


(defn read-log-trace [h domains]
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
							(= t :log_end) [{:id ss :inputs (persistent! v)} doms]
							(= (first t) :log_start)
								(do
									(close-log h)
									(throw (Exception. "New log started in the middle of existing log")))
							(and (= :i (first t)) (contains? bl (second t))) (recur v doms)
						 	(= :i (first t))
						 		(let [input (apply make-input (input-parts t))
											new-domains (expand-domain input doms)]
									(recur (conj! v input) new-domains))
						  :true (recur v doms))))
				(do
					(close-log h)
					(throw (Exception. "Log does not begin with :log_start term")))))
		nil))

(defn read-logs [path how-many blacklist indoms]
	(let [log (open-log path blacklist)
				domains (if (nil? indoms) (make-domains) indoms)]
		(loop [ts (transient (hash-map))
					 remaining how-many
					 doms domains]
			(if (or (= remaining :all) (> remaining 0))
				(if-let [[trace new-doms] (read-log-trace log doms)]
					(recur
					 (assoc! ts (:id trace) (:inputs trace))
					 (if (number? remaining) (- remaining 1) remaining)
					 new-doms)
					(do
						(close-log log)
						{:traces (persistent! ts) :domains doms}))
				(do
					(close-log log)
					{:traces (persistent! ts) :domains doms})))))

; (count (vals (:traces (time (read-logs "/Users/joe/Projects/game/xsb/logs/log.i.trace" 1 (hash-set :system :random) nil)))))

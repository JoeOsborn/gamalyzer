(ns gamalyzer.read.edn
	(:require [clojure.edn :refer [read]])
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

(defn read-log-trace [h]
	(if-let [start (read-log-entry h)]
		(let [fs (first start)
					ss (second start)
					bl (:blacklist h)]
			(if (= fs :log_start)
				; [:i :system :root (:game 0) :player_count (2 4 2)] ....
				; until :log_end
				; read the whole thing into a ... vector? cons list?
				; and then return it.
				(loop [v (transient (vector))]
					(let [t (read-log-entry h)]
						(cond
							(= t :log_end) {:id ss :inputs (persistent! v)}
							(= (first t) :log_start)
								(do
									(close-log h)
									(throw (Exception. "New log started in the middle of existing log")))
							:true
						 		(cond
								 	(and (= :i (first t)) (contains? bl (second t))) (recur v)
									:true (recur (conj! v t))))))
				(do
					(close-log h)
					(throw (Exception. "Log does not begin with :log_start term")))))
		nil))

(defn read-logs [path how-many blacklist]
	(let [log (open-log path blacklist)]
		(loop [ts (transient (hash-map))
					 remaining how-many]
			(if (> remaining 0)
				(if-let [trace (read-log-trace log)]
					(recur
					 (assoc! ts (:id trace) (:inputs trace))
					 (- remaining 1))
					(do
						(close-log log)
						(persistent! ts)))
				(do
					(close-log log)
					(persistent! ts))))))

(count (first (vals (time (read-logs "/Users/joe/Projects/game/xsb/logs/log.i.trace" 1 (hash-set :system :random))))))

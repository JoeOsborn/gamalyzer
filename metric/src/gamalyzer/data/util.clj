(ns gamalyzer.data.util
	(:require [gamalyzer.data.input :refer [make-trace]])
	(:gen-class :name gamalyzer.data.util
							:methods
							[^{:static true} [trim [gamalyzer.data.input.Trace int] gamalyzer.data.input.Trace]
							 ^{:static true} [stringify [Object] String]]))

(defn trim [trace len]
	(make-trace (:id trace) (vec (take len (:inputs trace)))))

(defn mappify [t]
  (cond
   (map? t) (into (hash-map) (map (fn [[k v]]
                                    [(mappify k) (mappify v)]) t))
   (coll? t) (map mappify t)
   true t))

(defn -stringify [a] (str (mappify a)))
(defn -trim [trace len] (trim trace len))

; Informal tests and usage examples.

#_(trim (make-trace "0" [1 2 3 4 5]) 3)

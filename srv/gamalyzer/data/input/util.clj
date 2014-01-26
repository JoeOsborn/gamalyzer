(ns gamalyzer.data.input.util
	(:require [gamalyzer.data.input :refer [make-trace]])
	(:gen-class :name gamalyzer.data.input.util
							:methods
							[^{:static true} [trim [gamalyzer.data.input.Trace int] gamalyzer.data.input.Trace]]))

(defn -trim [trace len]
	(make-trace (:id trace) (take len (:inputs trace))))

#_(-trim (make-trace "0" [1 2 3 4 5]) 3)


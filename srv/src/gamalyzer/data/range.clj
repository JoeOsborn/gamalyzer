(ns gamalyzer.data.range
	(:require [multiset.core :refer [multiset]]))

(defn range? [r]
	(and
	 (associative? r)
	 (contains? r :low)
	 (contains? r :high)
	 (contains? r :samples)))

; todo: replace with conj
(defn expand-range [r a]
	(if (range? r)
		{:low (min (:low r) a)
		 :high (max (:high r) a)
		 :samples (conj (:samples r) a)}))

(defn make-range
	([a] {:low a :high a :samples (multiset a)})
	([a b] (expand-range (make-range a) b)))

;(make-range 1)
;(range? (make-range 1))
;(expand-range (make-range 1) 1)
;(expand-range (make-range 1) 2)
;(expand-range (make-range 1) 0)
;(expand-range (expand-range (make-range 1) 0) 5)
;(expand-range (expand-range (make-range 1) 2) 0)

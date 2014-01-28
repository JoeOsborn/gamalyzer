(ns gamalyzer.data.range
  (:require [multiset.core :refer [multiset]]
						[clojure.core.typed :refer [ann ann-form ann-record]]))

(ann ^:no-check multiset.core/multiset (All [a] [a -> (clojure.lang.IPersistentSet a)]))

(ann-record Range [low :- Number
									 high :- Number
									 samples :- (clojure.lang.IPersistentSet Number)])
(defrecord Range [low high samples])

(ann range? [Any -> Boolean :filters {:then (is Range 0), :else (! Range 0)}])
(defn range? [r] (instance? Range r))

; todo: replace with conj
(ann expand-range [Range Number -> Range])
(defn expand-range [r a]
	(Range. (min (:low r) a)
					(max (:high r) a)
					(conj (:samples r) a)))

(ann make-range (Fn
								 [Number -> Range]
								 [Number Number -> Range]))
(defn make-range
  ([a] (Range. a a (multiset a)))
  ([a b] (expand-range (make-range a) b)))

(ann mean [Range -> Number])
(defn mean [r]
  (let [s (:samples r)
        c (count s)]
    (reduce (ann-form #(+ %1 (/ %2 c))
											[Number Number -> Number])
						0
						s)))

(ann rrange [Range -> Number])
(defn rrange [r] (- (:high r) (:low r)))

; Informal tests and usage examples.

;(make-range 1)
;(range? (make-range 1))
;(expand-range (make-range 1) 1)
;(expand-range (make-range 1) 2)
;(expand-range (make-range 1) 0)
;(expand-range (expand-range (make-range 1) 0) 5)
;(expand-range (expand-range (make-range 1) 2) 0)

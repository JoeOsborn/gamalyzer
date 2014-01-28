(ns gamalyzer.data.input
  (:require [multiset.core :refer [multiset? multiset]]
            [gamalyzer.data.range :refer [range? make-range expand-range]]
						[clojure.core.typed :refer [ann ann-form ann-record def-alias inst]]))

(def-alias ITerminal
	(U Number clojure.lang.Keyword))

(def-alias ICompound
	(U (clojure.lang.IPersistentVector ITerm)
		 (clojure.lang.ISeq ITerm)))

(def-alias ITerm "Input Term"
	(U ITerminal ICompound))

(def-alias TraceID "Trace ID"
	(U Number clojure.lang.Keyword String))

(def-alias Domain "Terminal Domain"
	(U gamalyzer.data.range.Range
		 (clojure.lang.IPersistentSet clojure.lang.Keyword)))

(def-alias DomainPath "Domain Path"
	(clojure.core.typed/NonEmptyVec clojure.core.typed/AnyInteger))

(def-alias DomainMap "Domain Map"
	(clojure.lang.IPersistentMap clojure.core.typed/AnyInteger (U DomainMap Domain)))

(def-alias DetDomainMap "Determinant->Domain Map"
	(clojure.lang.IPersistentMap ITerm DomainMap))

(def-alias MaybeDomain (U Domain nil))

(ann ^:no-check clojure.core/last [DomainPath -> clojure.core.typed/AnyInteger])
(ann ^:no-check clojure.core/sequential? [Any -> Boolean
																					:filters {:then (is clojure.lang.Seqable 0)
																										:else (! clojure.lang.Seqable 0)}])
(ann ^:no-check multiset.core/multiset (All [a] [a -> (clojure.lang.IPersistentSet a)]))
(ann ^:no-check multiset.core/multiset?
		 (All [a] [Any -> Boolean
							 :filters {:then (is (clojure.lang.IPersistentSet a) 0),
												 :else (! (clojure.lang.IPersistentSet a) 0)}]))
(ann ^:no-check clojure.core/update-in [DomainMap DomainPath (Fn [MaybeDomain -> Domain]) -> DomainMap])

(ann-record Input [time :- Number
									 player :- ITerm
									 det :- ICompound
									 vals :- ICompound])
(defrecord Input [time player det vals])
(ann-record Domains [domains :- DetDomainMap])
(defrecord Domains [domains])
(ann-record Trace [id :- TraceID
									 inputs :- (clojure.lang.IPersistentVector Input)])
(defrecord Trace [id inputs])
(ann-record Traces [traces :- (clojure.lang.IPersistentVector Trace)
										domains :- Domains])
(defrecord Traces [traces ^gamalyzer.data.input.Domains domains])

(ann make-input [Number ITerm ICompound ICompound -> Input])
(defn make-input [ts pl det inp] (Input. ts pl det inp))
(ann input? [Input -> Boolean :filters {:then (is Input 0), :else (! Input 0)}])
(defn input? [i] (instance? Input i))
(ann make-domains
		 (Fn [ -> Domains]
				 [DetDomainMap -> Domains]))
(defn make-domains
	([] (make-domains {}))
	([dds] (Domains. dds)))
(ann make-trace [TraceID (clojure.lang.IPersistentVector Input) -> Trace])
(defn make-trace [id inputs] (Trace. id inputs))
(ann make-traces [(clojure.lang.IPersistentVector Trace) Domains -> Traces])
(defn make-traces [traces domains] (Traces. traces domains))

(ann t [Input -> Number])
(defn t [i] (:time i))
(ann player [Input -> ITerm])
(defn player [i] (:player i))
(ann determinant [Input -> ICompound])
(defn determinant [i] (:det i))
(ann values [Input -> ICompound])
(defn values [i] (:vals i))
(ann parts [Input -> '[ICompound ICompound]])
(defn parts [i] [(determinant i) (values i)])

(ann merge-domains [DomainMap DomainPath ITerminal -> DomainMap])
(defn- merge-domains [dmap path belt]
	(update-in dmap
						 path
						 (fn [dom]
							 (cond
								(and (range? dom) (number? belt)) (expand-range dom belt)
								(range? dom) (throw (IllegalArgumentException. "Cannot extend a range with a keyword"))
								(number? belt) (throw (IllegalArgumentException. "Cannot extend a multiset with a number"))
								(multiset? dom) (conj dom belt)
								true (if (number? belt)
											 (make-range belt)
											 (multiset belt))))))

(ann get-domains [Domains ITerm -> DomainMap])
(defn get-domains [doms det]
	(get (:domains doms) det {}))

(ann get-domain [DomainMap DomainPath -> MaybeDomain])
(defn get-domain [dmap path]
  (let [ret (get-in dmap path)]
		(cond
		 (nil? ret) ret
		 (or (range? ret) (multiset? ret)) ret
		 :else (throw (IllegalArgumentException. "The given path must name a domain, not a domain map")))))

(ann increment-path [DomainPath -> DomainPath])
(defn- increment-path [path]
	(let [last-elt (ann-form (last path) clojure.core.typed/AnyInteger)
				next-path ((inst assoc DomainPath clojure.core.typed/AnyInteger clojure.core.typed/AnyInteger)
									 path
									 (dec (count path))
									 (inc last-elt))]
		(assert (not (empty? next-path)))
		next-path))

;;; Can't for the life of me figure out how to make conj work properly for this.
(ann ^:no-check append-path [DomainPath clojure.core.typed/AnyInteger -> DomainPath])
(defn- append-path [path n]
	((inst conj DomainPath clojure.core.typed/AnyInteger) path 0))

(ann expand-domains [DomainMap ICompound DomainPath -> DomainMap])
(defn- expand-domains [dds inp path]
	(let [h (ann-form (first inp) (clojure.core.typed/Option ITerm))
				tail (ann-form (rest inp) ICompound)
				next-path (increment-path path)]
		(cond
		 (nil? h) dds
		 (or (keyword? h)
				 (number? h)) (expand-domains (merge-domains dds path h)
																			tail
																			next-path)
		 (sequential? h) (expand-domains
											(expand-domains dds
																			h
																			(append-path path 0))
											tail
											next-path)
		 true (throw (IllegalArgumentException. "Bad input term")))))

(ann expand-domain [Input Domains -> Domains])
(defn expand-domain [input doms]
  (let [det (:det input)
        dds (get-domains doms det)
        new-dds (expand-domains dds (:vals input) [0])]
		(make-domains (assoc
										(ann-form (:domains doms) DetDomainMap)
										(ann-form det ITerm)
										(ann-form new-dds DomainMap)))))

(ann expand-domain* [(clojure.lang.Seqable Input) Domains -> Domains])
(defn expand-domain* [inputs doms]
  (reduce (ann-form #(expand-domain %2 %1)
										(Fn [Domains Input -> Domains]))
					doms
					inputs))

(ann expand-domain** [(clojure.lang.Seqable Trace) Domains -> Domains])
(defn expand-domain** [traces doms]
  (reduce (ann-form #(expand-domain* (:inputs %2) %1)
										(Fn [Domains Trace -> Domains]))
					doms
					(vec traces)))

; Informal tests and usage examples.

;(expand-domain (make-input 0 0 :a '(1)) (make-domains))
;(expand-domain (make-input 0 0 :a '((1))) (make-domains))
;(expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains))
;(expand-domain (make-input 1 0 :a '((2) :b (3))) (expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains)))
#_(expand-domain (make-input 1 0 :b '((1) :b (2))) (expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains)))
#_(expand-domain (make-input 1 0 :a [[:p 1] :b [2]]) (expand-domain (make-input 0 0 :a [[:p 1] :a [2]]) (make-domains)))

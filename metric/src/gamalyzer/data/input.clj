(ns gamalyzer.data.input
  (:require [multiset.core :refer [multiset? multiset]]
            [gamalyzer.data.range :refer [range? make-range expand-range]]))

(defrecord Input [time player det vals])
(defrecord Domains [])
(defrecord Trace [id inputs])
(defrecord Traces [traces ^gamalyzer.data.input.Domains domains])

(defn make-input [ts pl det inp] (Input. ts pl det inp))
(defn input? [i] (instance? Input i))
(defn make-domains [] (Domains.))
(defn make-trace [id inputs] (Trace. id inputs))
(defn make-traces [traces domains] (Traces. traces domains))

(defn t [i] (:time i))
(defn player [i] (:player i))
(defn determinant [i] (:det i))
(defn values [i] (:vals i))
(defn parts [i] [(determinant i) (values i)])

(defn- merge-domains [dds path belt]
  (update-in
   dds
   path
   #(cond
     (range? %1) (expand-range %1 belt)
     (multiset? %1) (conj %1 belt)
     (nil? %1) (if (number? belt) (make-range belt) (multiset belt)))))

(defn get-domains [doms det]
  (get doms det))

(defn get-domain [dds path]
  (get-in dds path))

(defn- expand-domains [dds inp path]
  (let [h (first inp)
        tail (rest inp)
        end (dec (count path))
        next-path (assoc path end (inc (get path end)))]
    (cond
     (nil? h) dds
     (sequential? h)
       (expand-domains
        (expand-domains dds h (conj path 0))
        tail
        next-path)
     true
       (expand-domains
        (merge-domains dds path h)
        tail
        next-path))))

(defn expand-domain [input doms]
  (let [det (:det input)
        dds (get doms det)
        new-dds (expand-domains dds (:vals input) [0])]
    (assoc doms det new-dds)))

(defn expand-domain* [inputs doms]
  (reduce #(expand-domain %2 %1) doms inputs))

(defn expand-domain** [traces doms]
  (reduce #(expand-domain* (:inputs %2) %1) doms (vec traces)))

;(expand-domain (make-input 0 0 :a '(1)) (make-domains))
;(expand-domain (make-input 0 0 :a '((1))) (make-domains))
;(expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains))
;(expand-domain (make-input 1 0 :a '((2) :b (3))) (expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains)))
#_(expand-domain (make-input 1 0 :b '((1) :b (2))) (expand-domain (make-input 0 0 :a '((1) :a (2))) (make-domains)))
#_(expand-domain (make-input 1 0 :a [[:p 1] :b [2]]) (expand-domain (make-input 0 0 :a [[:p 1] :a [2]]) (make-domains)))

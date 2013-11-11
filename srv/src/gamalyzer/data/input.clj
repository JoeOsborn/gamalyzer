(ns gamalyzer.data.input
  (:require [multiset.core :refer [multiset? multiset]]
            [gamalyzer.data.range :refer [range? make-range expand-range]]))

(defn make-input [det inp] {:det det :vals inp})
(defn input? [i] (and (associative? i) (contains? i :det) (contains? i :vals)))
(defn make-domains [] {})

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
        end (- (count path) 1)
        next-path (assoc path end (+ 1 (get path end)))]
    (cond
     (nil? h) dds
     (seq? h)
     (expand-domains
      (expand-domains dds h (conj path 1))
      tail
      next-path)
     :true
     (expand-domains
      (merge-domains dds path h)
      tail
      next-path))))

(defn expand-domain [input doms]
  (let [det (:det input)
        dds (get doms det)
        new-dds (expand-domains dds (:vals input) [0])]
    (assoc doms det new-dds)))

;(expand-domain (make-input :a '(1)) (make-domains))
;(expand-domain (make-input :a '((1))) (make-domains))
;(expand-domain (make-input :a '((1) :a (2))) (make-domains))
;(expand-domain (make-input :a '((2) :b (3))) (expand-domain (make-input :a '((1) :a (2))) (make-domains)))
;(expand-domain (make-input :b '((1) :b (2))) (expand-domain (make-input :a '((1) :a (2))) (make-domains)))

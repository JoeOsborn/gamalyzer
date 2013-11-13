(ns gamalyzer.cmp.i-i
  (:require [gamalyzer.data.input :refer [parts make-domains make-input expand-domain get-domains get-domain]]
            [gamalyzer.data.range :refer [rrange]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.test :refer [is]]))

; takes game traces as input:
; {:traces ts :doms doms}
;   ts = {uuid {:id uuid :inputs is}}*
;     is = [input*]
;       input = (parts input) = [det vals]

(declare vs-diss)

(defn- vs-diss-vecs [vs1 vs2 path doms]
  (let [l1 (count vs1)
        l2 (count vs2)
        each-diss (/ 1.0 l1)]
    (when-not (= l1 l2) (throw (IllegalArgumentException. "Sequences of unequal length")))
    (second
     (reduce (fn [[i d] [v1 v2]]
               (let [new-d (* (vs-diss v1 v2 (conj path i) doms) each-diss)]
                 [(inc i) (+ d new-d)]))
             [0 0]
             (map vector vs1 vs2)))))

(defn- vs-diss-lists [vs1 vs2 path doms]
  (let [l1 (count vs1)
        l2 (count vs2)]
    (when-not (= l1 l2) (throw (IllegalArgumentException. "Sequences of unequal length")))
    (second
     (reduce (fn [[i d] [v1 v2]]
               (let [this-diss (/ (- l1 i) (inc l1))
                     new-d (* (vs-diss v1 v2 (conj path i) doms) this-diss)]
                 [(inc i) (+ d new-d)]))
             [0 0]
             (map vector vs1 vs2)))))

(defn- vs-diss-nums [v1 v2 path doms]
  (let [r (get-domain doms path)
        rng (rrange r)
        d (abs (- v1 v2))]
    (if (= d 0)
      0
      (/ d rng))))

(defn- vs-diss-prim [v1 v2] (if (= v1 v2) 0.0 1.0))

(defn- vs-diss [vs1 vs2 path doms]
  (cond
   (and (vector? vs1) (vector? vs2)) (vs-diss-vecs vs1 vs2 path doms)
   (and (sequential? vs1) (sequential? vs2)) (vs-diss-lists vs1 vs2 path doms)
   (and (number? vs1) (number? vs2)) (vs-diss-nums vs1 vs2 path doms)
   (and (not (sequential? vs1)) (not (sequential? vs2))) (vs-diss-prim vs1 vs2)
   true (throw (Exception. (str "Could not find distance between " (.toString vs1) " and " (.toString vs2))))))

(defn diss [i1 i2 doms]
  (let [[_ _ d1 vs1] (parts i1)
        [_ _ d2 vs2] (parts i2)]
    (if-not (= d1 d2) 1.0
      (* (vs-diss vs1 vs2 [] (get-domains doms d1)) 0.8))))

(is (= 0.0 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :a [:a]) (make-domains))))
(is (= 0.8 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :a [:b]) (make-domains))))
(is (= 1.0 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :b [:a]) (make-domains))))
(is (= 0.0 (diss (make-input 1 0 [:a :a] [:a]) (make-input 0 0 [:a :a] [:a]) (make-domains))))
(is (= 1.0 (diss (make-input 1 0 [:a :a] [:a]) (make-input 0 0 [:a :b] [:a]) (make-domains))))
(is (= 0.4 (diss (make-input 1 0 :a [:a :a]) (make-input 0 0 :a [:a :b]) (make-domains))))
(is (< 0.26 (diss (make-input 1 0 :a '(a a)) (make-input 0 0 :a '(a b)) (make-domains)) 0.27))
(is (< 0.53 (diss (make-input 1 0 :a '(a a)) (make-input 0 0 :a '(b a)) (make-domains)) 0.54))
(is (= 0.0 (diss (make-input 1 0 :a [:a [:a]]) (make-input 0 0 :a [:a [:a]]) (make-domains))))
(is (= 0.4 (diss (make-input 1 0 :a [:a [:a]]) (make-input 0 0 :a [:a [:b]]) (make-domains))))
(is (= 0.0 (diss (make-input 1 0 :a [:a [:a :a]]) (make-input 0 0 :a [:a [:a :a]]) (make-domains))))
(is (= 0.2 (diss (make-input 1 0 :a [:a [:a :a]]) (make-input 0 0 :a [:a [:a :b]]) (make-domains))))

(is (< 0.13 (diss (make-input 1 0 :a [:a '(a a)]) (make-input 0 0 :a [:a '(a b)]) (make-domains)) 0.14))

(let [ia1 (make-input 0 0 :a [1])
      ia2 (make-input 1 0 :a [2])
      ia3 (make-input 2 0 :a [3])
      ib1 (make-input 0 0 :b [0])
      ib2 (make-input 1 0 :b [5])
      ib3 (make-input 2 0 :b [10])
      ib4 (make-input 3 0 :b [1])
      ds (reduce #(expand-domain %2 %1) (make-domains) [ia1 ia2 ia3 ib1 ib2 ib3 ib4])]
  (is (= 0.0 (diss ia1 ia1 ds)))
  (is (= 0.4 (diss ia1 ia2 ds)))
  (is (< 0.080 (diss ib1 ib4 ds) 0.081)))

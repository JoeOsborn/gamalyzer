(ns gamalyzer.cmp.ii
  (:require [gamalyzer.data.input :refer [parts make-domains make-input expand-domain get-domains get-domain]]
            [gamalyzer.data.range :refer [rrange]]
            [gamalyzer.read.edn :refer [read-logs]]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.test :refer [is]]))

; takes game traces as input:
; {:traces ts :doms doms}
;   ts = {uuid {:id uuid :inputs is}}*
;     is = [input*]
;       input = (parts input) = [det vals]

(declare vs-diss)

(defn- vs-diss-vecs ^double [vs1 vs2 path doms]
  (let [l1 (double (count vs1))
       ; l2 (count vs2)
        each-diss (double (/ 1.0 l1))]
   ; (when-not (= l1 l2) (throw (IllegalArgumentException. "Sequences of unequal length")))
    (loop [i (int 0) d (double 0.0)]
      (if (< i l1)
        (let [v1 (vs1 i)
              v2 (vs2 i)
              new-d (double (* (vs-diss v1 v2 (conj path i) doms) each-diss))]
          (recur (inc i) (+ d new-d)))
        d))))

(defn- vs-diss-lists ^double [vs1 vs2 path doms]
  (let [l1 (double (count vs1))
        denom (/ (* l1 (inc l1)) 2)
        one (double 1.0)]
    (loop [i (int 0)
           ii (double l1)
           d (double 0.0)
           vs1r vs1
           vs2r vs2]
      (if (empty? vs1r)
        d
        (let [v1 (first vs1r)
              v2 (first vs2r)
              this-diss (double (/ ii denom))
              new-d (* (vs-diss v1 v2 (conj path i) doms) this-diss)]
          (recur (inc i)
                 (- ii one)
                 (+ d new-d)
                 (rest vs1r)
                 (rest vs2r)))))))

(defn- vs-diss-nums ^double [v1 v2 path doms]
  (let [r (get-domain doms path)
        rng (double (rrange r))
        d (double (abs (- v1 v2)))]
    (if (== d 0.0) 0.0 (/ d rng))))

(defn- vs-diss-prim ^double [v1 v2] (if (= v1 v2) 0.0 1.0))

(defn vs-diss ^double [vs1 vs2 path doms]
  (cond
   (= vs1 vs2) 0.0
   (and (vector? vs1) (vector? vs2)) (vs-diss-vecs vs1 vs2 path doms)
   (and (sequential? vs1) (sequential? vs2)) (vs-diss-lists vs1 vs2 path doms)
   (and (number? vs1) (number? vs2)) (vs-diss-nums vs1 vs2 path doms)
   (and (not (sequential? vs1)) (not (sequential? vs2))) (vs-diss-prim vs1 vs2)
   true (throw (Exception. (str "Could not find distance between " (.toString vs1) " and " (.toString vs2))))))

(defn diss ^double [i1 i2 doms]
  (let [[d1 vs1] (parts i1)
        [d2 vs2] (parts i2)]
    (if-not (= d1 d2) 1.0
      (* (vs-diss vs1 vs2 [] (get-domains doms d1)) 0.8))))

#_(let [logs (gamalyzer.read.mario/sample-data)
      vs (:traces logs)
      doms (:domains logs)
      maria (get vs "replay_MariaJesus_38")
      emil (get vs "replay_Emil_38")]
  [(:vals (second (:inputs maria)))
   (:vals (second (:inputs emil)))
   (vs-diss (:vals (second (:inputs maria))) (:vals (second (:inputs emil)))
            []
            (get-domains doms [:move]))])


#_(is (= 0.0 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :a [:a]) (make-domains))))
#_(is (= 0.8 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :a [:b]) (make-domains))))
#_(is (= 1.0 (diss (make-input 1 0 :a [:a]) (make-input 0 0 :b [:a]) (make-domains))))
#_(is (= 0.0 (diss (make-input 1 0 [:a :a] [:a]) (make-input 0 0 [:a :a] [:a]) (make-domains))))
#_(is (= 1.0 (diss (make-input 1 0 [:a :a] [:a]) (make-input 0 0 [:a :b] [:a]) (make-domains))))
#_(is (= 0.4 (diss (make-input 1 0 :a [:a :a]) (make-input 0 0 :a [:a :b]) (make-domains))))
#_(is (< 0.26 (diss (make-input 1 0 :a '(a a)) (make-input 0 0 :a '(a b)) (make-domains)) 0.27))
#_(is (< 0.53 (diss (make-input 1 0 :a '(a a)) (make-input 0 0 :a '(b a)) (make-domains)) 0.54))
#_(is (= 0.0 (diss (make-input 1 0 :a [:a [:a]]) (make-input 0 0 :a [:a [:a]]) (make-domains))))
#_(is (= 0.4 (diss (make-input 1 0 :a [:a [:a]]) (make-input 0 0 :a [:a [:b]]) (make-domains))))
#_(is (= 0.0 (diss (make-input 1 0 :a [:a [:a :a]]) (make-input 0 0 :a [:a [:a :a]]) (make-domains))))
#_(is (= 0.2 (diss (make-input 1 0 :a [:a [:a :a]]) (make-input 0 0 :a [:a [:a :b]]) (make-domains))))

#_(is (< 0.13 (diss (make-input 1 0 :a [:a '(a a)]) (make-input 0 0 :a [:a '(a b)]) (make-domains)) 0.14))

#_(let [ia1 (make-input 0 0 :a [1])
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

#_(let [i1 {:det (list [:play_card 2] (list :library 0) :keep),
          :vals (list [:p 2] [:p 2] :library)}
      i2 {:det (list [:play_card 2] (list :library 0) :keep),
          :vals (list [:p 2] [:p 2] :library)}
      doms (:domains (read-logs "/Users/jcosborn/Projects/game/xsb/logs/log.i.trace"
                                7
                                (hash-set :system :random)
                                nil))]
  (is (not (Double/isNaN (diss i1 i2 doms)))))

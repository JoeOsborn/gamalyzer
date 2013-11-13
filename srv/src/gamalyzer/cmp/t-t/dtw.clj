(ns gamalyzer.cmp.t-t.dtw
  (:require [gamalyzer.cmp.i-i :as i-i]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.core.cache :as cache]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.edn :refer [read-logs]]))

;;;; Dynamic time warping (dynamic programming implementation)
;;;; Berndt & Clifford

(def warp-w 50)

(defn distance [c s1 i s2 j doms]
  (if (cache/has? c [i j])
    (do
     (cache/hit c [i j])
     (cache/lookup c [i j]))
    (cond
     (= i 0) (do (cache/miss c [i j] j) j)
     (= j 0) (do (cache/miss c [i j] i) i)
     (>= (abs (- i j)) warp-w) (let [max-rem (+ i j)]
                                 (cache/miss c [i j] max-rem)
                                 max-rem)
     true (let [i1 (get s1 i)
                i2 (get s2 j)
                d (i-i/diss i1 i2 doms)
                dtw (+ d (min (distance c s1 (dec i) s2 j doms)
                              (distance c s1 (dec i) s2 (dec j) doms)
                              (distance c s1 i s2 (dec j) doms)))]
            (cache/miss c [i j] dtw)
            dtw))))

(defn diss [s1 s2 doms]
  (if (= (:uuid s1) (:uuid s2))
    0
    (let [c (cache/soft-cache-factory {})
          t1 (:trace s1)
          t2 (:trace s2)
          l1 (count t1)
          l2 (count t2)
          dist (distance c t1 (dec l1) t2 (dec l2) doms)
          max-dist (+ l1 l2)]
      (/ dist max-dist))))

(time (doall (let [logs (read-logs "/Users/jcosborn/Projects/game/xsb/logs/log.i.trace" 10 (hash-set :system :random) nil)
                   vs (:traces logs)
                   doms (:domains logs)
            vss (vec (keys vs))]
    (map (fn [[a b]]
           (vector [a b] (double (diss {:uuid (get vss a), :trace (get vs (get vss a))} {:uuid (get vss b), :trace (get vs (get vss b))} doms))))
         (cartesian-product (range 0 10) (range 0 10))))))

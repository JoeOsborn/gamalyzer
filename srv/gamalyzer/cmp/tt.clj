(ns gamalyzer.cmp.tt
  (:require [gamalyzer.cmp.tt.cced :as dist]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.synth :refer [read-logs]]
            [clojure.core.matrix :refer [new-array mget mset! set-current-implementation
                                         dimension-count shape]])
  (:import [java.lang Double]))

(set-current-implementation :vectorz)
(defn diss-t [s1 s2 doms] (dist/diss-t s1 s2 doms))

(defn best-pivot-distance [mat pivots n]
  ; find among the |pivots| pivots in mat the lowest distance to trace #n
  (let [rng (range 0 (count pivots))
        distances (map #(mget mat % n (dec (dimension-count mat 2))) rng)
        min-distance (apply min distances)]
    min-distance))

(defn not-a-pivot? [pivots t]
  (not (some #{(:id t)} pivots)))

(defn maxmin-index [pivots mat traces]
  (let [rng (range 0 (count traces))
        non-pivot-indices (filter #(not-a-pivot? pivots (get traces %)) rng)
        maxmin (apply max-key #(best-pivot-distance mat pivots %) non-pivot-indices)]
    maxmin))

(defn select-pivot [pivots mat traces]
  (if (empty? pivots)
    (first traces)
    ; find the non-pivot trace #n with the largest "best distance" to every pivot up to k
    (get traces (maxmin-index pivots mat traces))))

; fills a 2d submatrix at [pivot-index,0..|traces|,0..max-length] with the distances
; of each trace up to max-length.
(defn calc-pivot-diffs! [mat pivot-index pivot traces doms]
  (doseq [:let [max-length (dimension-count mat 2)]
          j (range 0 (count traces))
          :let [sample (get traces j)
                distances (diss-t pivot sample doms)
                how-many (dimension-count distances 0)]
          d (range 0 max-length)
          :let [dist (if (< d how-many) (mget distances d) (last distances))]]
    (mset! mat pivot-index j d dist)))

(defn pivot-distances [k traces doms]
  (let [n (count traces)
        ts (vec (vals traces))
        max-length (apply max (map #(count (:inputs %)) ts))
        mat0 (new-array [k n max-length])]
    (reduce
     (fn [[pivots mat] ki]
         (let [pivot (select-pivot pivots mat ts)]
           (calc-pivot-diffs! mat ki pivot ts doms)
           [(conj pivots (:id pivot)) mat]))
     [[] mat0]
     (range 0 k))))

;in-progress: refactor/simplify/fix this module, then use mdsj on the pivots, then use it as static data for some clojurescript/d3 thing. NB: similarity scores on leads in the final vis must be "up to this point" -- i.e. the first K steps of the best path. and latch that score for shorter vs longer paths? it's hard to know what to do for sure.

(defn tst [n k]
  (time (doall
         (let [dur 25
               logs (read-logs [[:a (/ n 3) dur {[1 [:a] [:a]] 1.0}]
                                [:b (/ n 3) dur {[1 [:b] [:a]] 1.0}]
                                [:c (/ n 3) dur {[1 [:a] [:b]] 1.0}]]
                               (hash-set :system :random)
                               nil)
               vs (:traces logs)
               doms (:domains logs)
               [pivots mat] (pivot-distances k vs doms)]
           [(map #(get vs %) pivots) mat]))))

(tst 100 10)


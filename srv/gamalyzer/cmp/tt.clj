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
  (not-any? #(= % t) pivots))

(defn maxmin-index [pivots mat traces]
  (let [rng (range 0 (count traces))
        non-pivot-indices (filter #(not-a-pivot? pivots %) rng)
        maxmin (apply max-key #(best-pivot-distance mat pivots %) non-pivot-indices)]
    maxmin))

(defn select-pivot [pivots mat traces]
  (if (empty? pivots)
    (rand-int (count traces))
    ; find the non-pivot trace #n with the largest "best distance" to every pivot up to k
    (maxmin-index pivots mat traces)))

; fills a 2d submatrix at [pivot-index,0..|traces|,0..max-length] with the distances
; of each trace up to max-length.
(defn calc-pivot-diffs! [mat pivot-index pivot traces doms]
  (doseq [:let [max-length (dimension-count mat 2)]
          j (range 0 (count traces))
          :let [sample (nth traces j)
                distances (diss-t pivot sample doms)
                how-many (dimension-count distances 0)]
          d (range 0 max-length)
          :let [dist (if (< d how-many)
                       (mget distances d)
                       (last distances))]]
    (mset! mat pivot-index j d dist)))

(defn pivot-distances [k traces doms]
  (let [n (count traces)
        max-length (apply max (map #(count (:inputs %)) traces))
        mat0 (new-array [k n max-length])]
    (reduce
     (fn [[pivots mat] ki]
         (let [pivot-index (select-pivot pivots mat traces)
               pivot (nth traces pivot-index)]
           (calc-pivot-diffs! mat ki pivot traces doms)
           [(conj pivots pivot-index) mat]))
     [[] mat0]
     (range 0 k))))

(defn tst [n k]
  (time (doall
         (let [dur 43
               logs (read-logs [[:a (/ n 3) dur {[1 [:a] [:a]] 1.0}]
                                [:b (/ n 3) dur {[1 [:b] [:a]] 1.0}]
                                [:c (/ n 3) dur {[1 [:a] [:b]] 1.0}]]
                               (hash-set :system :random)
                               nil)
               vs (:traces logs)
               doms (:domains logs)
               [pivots mat] (pivot-distances k vs doms)]
           [(map :id (map #(nth vs %) pivots)) mat]))))

(tst 10 10)

(let [traces [{:similar-count 53, :id "d73114ff-a682-4709-ba0a-f0f061e2f74f", :inputs [{:time 0, :player 1, :det [:a], :vals [:a]} {:time 1, :player 1, :det [:a], :vals [:a]} {:time 2, :player 1, :det [:a], :vals [:a]} {:time 3, :player 1, :det [:b], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:a]}], :label :ab} {:similar-count 33, :id "57e06a91-a650-4532-92d7-b19dae4d096e", :inputs [{:time 0, :player 1, :det [:b], :vals [:a]} {:time 1, :player 1, :det [:b], :vals [:a]} {:time 2, :player 1, :det [:b], :vals [:a]} {:time 3, :player 1, :det [:b], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:b]}], :label :bc} {:similar-count 16, :id "189b789c-78a3-4934-a1ac-da2fa5c5ccd8", :inputs [{:time 0, :player 1, :det [:a], :vals [:b]} {:time 1, :player 1, :det [:a], :vals [:b]} {:time 2, :player 1, :det [:a], :vals [:b]} {:time 3, :player 1, :det [:a], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:a]}], :label :ac}]
      [pivots pivot-mat] (pivot-distances 3 traces (gamalyzer.data.input/expand-domain** traces (gamalyzer.data.input/make-domains)))]
  (map clojure.core.matrix/to-nested-vectors (clojure.core.matrix/slices pivot-mat 2)))

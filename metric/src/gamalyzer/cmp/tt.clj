(ns gamalyzer.cmp.tt
  (:require [gamalyzer.cmp.tt.cced :as dist]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.core.matrix :refer [new-array mget mset!
                                         dimension-count shape eseq]])
  (:import [java.lang Double])
  (:gen-class :name gamalyzer.cmp.tt
              :methods
              [^{:static true} [diss [gamalyzer.data.input.Trace gamalyzer.data.input.Trace gamalyzer.data.input.Domains int] double]
							 ^{:static true} [dissimilarities [gamalyzer.data.input.Trace gamalyzer.data.input.Trace gamalyzer.data.input.Domains int] doubles]]))

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
        mat0 (new-array :vectorz [k n max-length])]
    (reduce
     (fn [[pivots mat] ki]
         (let [pivot-index (select-pivot pivots mat traces)
               pivot (nth traces pivot-index)]
           (calc-pivot-diffs! mat ki pivot traces doms)
           [(conj pivots pivot-index) mat]))
     [[] mat0]
     (range 0 k))))

(defn -diss [a b doms w] (dist/with-warp-window w (dist/diss a b doms)))
(defn -dissimilarities [a b doms w]
  (double-array (eseq (dist/with-warp-window (dist/diss-t a b doms)))))

; Informal tests and usage examples.

#_(defn tst [n k]
  (time (doall
         (let [dur 43
							 logs (gamalyzer.read.synth/sample-data
										 [[:a (/ n 3) dur {[1 [:a] [:a]] 1.0}]
											[:b (/ n 3) dur {[1 [:b] [:a]] 1.0}]
											[:c (/ n 3) dur {[1 [:a] [:b]] 1.0}]]
										 (hash-set :system :random)
										 nil)
               vs (:traces logs)
               doms (:domains logs)
               disss (-dissimilarities (first vs) (second vs) doms)]
           disss))))
#_(tst 10 10)
#_(defn tst [n k]
  (time (doall
         (let [dur 43
               logs (gamalyzer.read.synth/sample-data
										 [[:a (/ n 3) dur {[1 [:a] [:a]] 1.0}]
											[:b (/ n 3) dur {[1 [:b] [:a]] 1.0}]
											[:c (/ n 3) dur {[1 [:a] [:b]] 1.0}]]
										 (hash-set :system :random)
										 nil)
               vs (:traces logs)
               doms (:domains logs)
               [pivots mat] (pivot-distances k vs doms)]
           [(map :id (map #(nth vs %) pivots)) mat]))))

#_(tst 10 10)

#_(let [traces [{:similar-count 53, :id "d", :inputs [{:time 0, :player 1, :det [:a], :vals [:a]} {:time 1, :player 1, :det [:a], :vals [:a]} {:time 2, :player 1, :det [:a], :vals [:a]} {:time 3, :player 1, :det [:b], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:a]}], :label :ab} {:similar-count 33, :id "5", :inputs [{:time 0, :player 1, :det [:b], :vals [:a]} {:time 1, :player 1, :det [:b], :vals [:a]} {:time 2, :player 1, :det [:b], :vals [:a]} {:time 3, :player 1, :det [:b], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:b]}], :label :bc} {:similar-count 16, :id "1", :inputs [{:time 0, :player 1, :det [:a], :vals [:b]} {:time 1, :player 1, :det [:a], :vals [:b]} {:time 2, :player 1, :det [:a], :vals [:b]} {:time 3, :player 1, :det [:a], :vals [:a]} {:time 4, :player 1, :det [:a], :vals [:a]}], :label :ac}]
      [pivots pivot-mat] (pivot-distances 3 traces (gamalyzer.data.input/expand-domain** traces (gamalyzer.data.input/make-domains)))]
  (map clojure.core.matrix/to-nested-vectors (clojure.core.matrix/slices pivot-mat 2)))

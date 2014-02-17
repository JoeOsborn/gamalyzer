(ns gamalyzer.data
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]
            [gamalyzer.read.edn :as edn]
						[gamalyzer.read.synth :as synth]
						[gamalyzer.read.mario :as mario]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [gamalyzer.cmp.tt.cced :refer [with-warp-window]]
						[gamalyzer.data.util :refer [mappify]]
            [clojure.core.matrix :refer [mutable slices to-nested-vectors new-array
                                         mget mset! shape square dimension-count
                                         submatrix to-nested-vectors get-row]])
  (:import [java.util UUID]
           [mdsj MDSJ]))

(defn assign-vec-at! [m & indices-vec]
  (let [vct (last indices-vec)
        indices (drop-last indices-vec)]
    (doseq [i (range 0 (dimension-count vct 0))
            :let [v (mget vct i)]]
      (apply mset! m (concat indices [i v])))
    m))

(defn kxnxd->kxkxd [kxnxd pivot-ids traces]
  ;each 1d of mat matches up with an entry in pivots
  ;each 2d of mat matches up with an entry in traces
  ;make a new matrix with 1ds and 2ds matching up with pivots.
  ;so we need a mapping from pivots->traces
  (let [trace-indices (zipmap (map :id traces) (range))
        pivot-trace-indices (map #(get trace-indices %) pivot-ids)
        kxkxd (new-array :vectorz [(count pivot-ids)
																	 (count pivot-ids)
																	 (dimension-count kxnxd 2)])]
    (doseq [pi (range 0 (count pivot-trace-indices))
            pj (range 0 (count pivot-trace-indices))
            :let [pti (nth pivot-trace-indices pj)
                  mat-kxn (get-row
													 (get-row
														(submatrix kxnxd [[pi 1] [pti 1] nil])
														0)
													 0)]]
      (assign-vec-at! kxkxd pi pj mat-kxn))
    kxkxd))

(defn min-pivot-index [col-n]
	(apply min-key
				 #(mget col-n % (dec (dimension-count col-n 1)))
				 (range 0 (dimension-count col-n 0))))

(defn tally-similars [mat pivot-ids traces]
  (reduce (fn [sims col-n]
            (let [min-pivot (min-pivot-index col-n)
                  min-pivot-id (nth pivot-ids min-pivot)
                  so-far (get sims min-pivot-id)]
              (assoc sims
								min-pivot-id (if so-far (inc so-far) 1))))
          {}
          (slices mat 1)))

(defn normalize-and-align [positions]
  (let [low (apply min positions)
        high (apply max positions)
        delta (- high low)
        scale (/ 1.0 delta)
        scaled (map #(* % scale) positions)
        mean (/ (reduce + 0 scaled) (count scaled))
        zero-centered (map #(- % mean) scaled)
        zlow (apply min zero-centered)
        half-centered (map #(- % zlow) zero-centered)]
    half-centered))

(defn x-coords [kxk]
  (normalize-and-align
	 (vec (first (MDSJ/stressMinimization
								(into-array (map double-array kxk))
								1)))))

(defn noisify [pct kvs]
  ;reduce likelihood of each v by pct
  (let [new-norm (+ 1.0 pct)
				renorm-keyvals (fn [k]
												 [k (/ (+ (or (get kvs k) 0) (/ pct 3))
															 new-norm)])]
    (conj (into (hash-map)
                 (map (fn [[k v]]
                        [k (/ v new-norm)])
                      kvs))
					(map renorm-keyvals
							 [[1 [:a] [:a :a]]
								[1 [:a] [:a :b]]
								[1 [:a] [:b :b]]]))))

(defn synthetic-models [noise]
  (let [how-many 20
        how-long-mean 30
        how-long [(* how-long-mean 0.9) (* how-long-mean 1.1)]]
    [[:a how-many how-long (noisify noise {[1 [:a] [:a :a]] 1.0
                                           [1 [:a] [:a :b]] 0.0
                                           [1 [:a] [:b :b]] 0.0})]
     [:b how-many how-long (noisify noise {[1 [:a] [:a :a]] 0.0
                                           [1 [:a] [:a :b]] 1.0
                                           [1 [:a] [:b :b]] 0.0})]
     [:c how-many how-long (noisify noise {[1 [:a] [:a :a]] 0.0
                                           [1 [:a] [:a :b]] 0.0
                                           [1 [:a] [:b :b]] 1.0})]]))

(defn game-data [game lev]
  (cond
   (= game "mario") (mario/sample-data lev)
   (= game "refraction") (edn/read-logs (str "resources/traces/refraction/refraction." lev ".i.trace"))
   (= game "edn") (edn/sample-data)
   (= game "synthetic") (synth/sample-data (synthetic-models (get {0 0.0, 1 0.2903, 2 0.81815, 3 2.07692} lev)))))

(defn find-pivots [k vs doms]
  (let [n (count vs)
        k (min k n)
        [pivot-indices mat] (pivot-distances k vs doms)]
    [(map #(nth vs %) pivot-indices) mat]))

(defn process-data [logs k]
  (let [vs (:traces logs)
        doms (:domains logs)
        [pivots mat] (find-pivots k vs doms)
        pivot-ids (map :id pivots)
        msq (square mat)
        pivot-mat (kxnxd->kxkxd msq pivot-ids vs)
        similars (tally-similars msq pivot-ids vs)
        pivot-diffs-t (map to-nested-vectors (slices pivot-mat 2))]
    [(map #(or (:label %) (:id %)) pivots)
     (map #(assoc % :similar-count (or (get similars (:id %)) 0)) pivots)
     pivot-diffs-t
     (x-coords (last pivot-diffs-t))]))

(defn test-data [game lev k]
  (let [logs (game-data game lev)]
    (process-data logs k)))

(defn data [game ctx]
	(println "game is" game)
	(let [req (:query-params (:request ctx))
				lev (or (and (get req "level") (read-string (get req "level")))
								(get {"mario" 0 "refraction" 5 "synthetic" 0 "edn" 5} game))
				k (or (and (get req "k") (read-string (get req "k")))
							10)
				window (or (and (get req "window") (read-string (get req "window")))
									 20)]
		(with-warp-window window
			(mappify (test-data game lev k)))))


(ns gamalyzer.rsrc.data
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]
            [gamalyzer.read.synth :refer [read-logs]]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [clojure.core.matrix :refer [set-current-implementation mutable
                                         slices to-nested-vectors new-array
                                         mget mset! shape square dimension-count
                                         submatrix to-nested-vectors get-row]])
  (:import [java.util UUID]
           [mdsj MDSJ]))

(set-current-implementation :vectorz)

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
        kxkxd (new-array [(count pivot-ids) (count pivot-ids) (dimension-count kxnxd 2)])]
    (doseq [pi (range 0 (count pivot-trace-indices))
            pj (range 0 (count pivot-trace-indices))
            :let [pti (nth pivot-trace-indices pj)
                  mat-kxn (get-row (get-row (submatrix kxnxd [[pi 1] [pti 1] nil]) 0) 0)]]
      ;(println "pi" pi "(" (:label (nth traces (nth pivot-trace-indices pi))) ")->" pti "(" (:label (nth traces pti)) "):" mat-kxn)
      (assign-vec-at! kxkxd pi pj mat-kxn))
    kxkxd))

(defn tally-similars [mat pivot-ids traces]
  (reduce (fn [sims col-n]
            (let [min-pivot (apply min-key
                                   #(mget col-n % (dec (dimension-count col-n 1)))
                                   (range 0 (dimension-count col-n 0)))
                  min-pivot-id (nth pivot-ids min-pivot)
                  so-far (get sims min-pivot-id)]
              (assoc sims min-pivot-id (if so-far (+ so-far 1) 1))))
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
  (normalize-and-align (vec (first (MDSJ/stressMinimization (into-array (map double-array kxk)) 1)))))


(defn test-data [n k dur]
  (let [logs (read-logs [[:a (/ n 4) [dur (* dur 1.25)] {[1 [:a] [:a]] 1.0}]
                         [:b (/ n 4) [dur (* dur 1.25)] {[1 [:b] [:a]] 1.0}]
                         [:c (/ n 4) [dur (* dur 1.25)] {[1 [:a] [:b]] 1.0}]
                         [:ac (/ n 4) [dur (* dur 1.25)] {[1 [:a] [:a]] 0.4
                                                          [1 [:a] [:b]] 0.4
                                                          [1 [:b] [:a]] 0.2}]]
                        (hash-set :system :random)
                        nil)
        vs (:traces logs)
        doms (:domains logs)
        [pivot-ids mat] (pivot-distances k vs doms)
        pivots (map #(get vs %) pivot-ids)
        msq (square mat)
        pivot-mat (kxnxd->kxkxd msq pivot-ids (vals vs))
        similars (tally-similars msq pivot-ids vs)
        pivot-diffs-t (map to-nested-vectors (slices pivot-mat 2))]
    [(map :label pivots)
     (map #(assoc % :similar-count (or (get similars (:id %)) 0)) pivots)
     pivot-diffs-t
     (x-coords (last pivot-diffs-t))]))

(test-data 100 10 5)

(defresource data []
	:available-media-types ["application/edn"]
	:handle-ok
		(fn [ctx] (test-data 100 10 30)))

(defroutes data-routes
  (ANY "/data" [] (data)))

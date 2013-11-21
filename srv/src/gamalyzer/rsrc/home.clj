(ns gamalyzer.rsrc.home
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
        pivot-trace-indices (sort (map #(get trace-indices %) pivot-ids))
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

(defn x-coords [kxk]
  (vec (first (MDSJ/stressMinimization (into-array (map double-array kxk)) 1))))


(defn test-data [n k dur]
  (let [logs (read-logs [[:a (/ n 3) dur {[1 [:a] [:a]] 1.0}]
                         [:b (/ n 3) dur {[1 [:b] [:a]] 1.0}]
                         [:c (/ n 3) dur {[1 [:a] [:b]] 1.0}]]
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
     (map #(assoc % :similar-count (get similars (:id %))) pivots)
     pivot-diffs-t
     (x-coords (last pivot-diffs-t))]))

(test-data 100 3 5)

(defresource home []
	:available-media-types ["application/json"]
	:handle-ok
		(fn [ctx] (test-data 100 10 15)))

(defroutes home-routes
  (ANY "/" [] (home)))

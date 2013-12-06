(ns gamalyzer.rsrc.data
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]
            [gamalyzer.read.edn :refer [read-logs]]
            [gamalyzer.read.mario :refer [sample-data]]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [gamalyzer.cmp.tt.cced :refer [with-warp-window]]
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

(defn game-data [game lev]
  (cond
   (= game :mario) (sample-data lev)
   (= game :refraction) (read-logs (str "resources/traces/refraction/refraction." lev ".i.trace"))))

(defn test-data [game lev k]
  (let [logs (game-data game lev)
        vs (:traces logs)
        doms (:domains logs)
        n (count vs)
        k (min k n)
        [pivot-indices mat] (pivot-distances k vs doms)
        pivots (map #(nth vs %) pivot-indices)
        pivot-ids (map :id pivots)
        msq (square mat)
        pivot-mat (kxnxd->kxkxd msq pivot-ids vs)
        similars (tally-similars msq pivot-ids vs)
        pivot-diffs-t (map to-nested-vectors (slices pivot-mat 2))]
;    (println "VS:" vs)
;    (println (gamalyzer.cmp.tt/diss-t (get vs "replay_MariaJesus_38") (get vs "replay_Emil_38") doms))
    [(map #(or (:label %) (:id %)) pivots)
     (map #(assoc % :similar-count (or (get similars (:id %)) 0)) pivots)
     pivot-diffs-t
     (x-coords (last pivot-diffs-t))]))

(defn mappify [t]
  (cond
   (map? t) (into (hash-map) (map (fn [[k v]]
                                    [(mappify k) (mappify v)]) t))
   (coll? t) (map mappify t)
   true t))

(mappify (test-data :mario 0 10))

(defresource data [game]
	:available-media-types ["application/edn"]
	:handle-ok
		(fn [ctx]
      (let [req (:query-params (:request ctx))
            lev (or (and (get req "level") (read-string (get req "level")))
                    (game {:mario 0 :refraction 5}))
            k (or (and (get req "k") (read-string (get req "k")))
                  10)
            window (or (and (get req "window") (read-string (get req "window")))
                       20)]
        (with-warp-window window
          (mappify (test-data game lev k))))))

(defroutes data-routes
  (ANY "/refraction" [] (data :refraction))
  (ANY "/mario" [] (data :mario)))

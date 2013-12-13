(ns gamalyzer.data.analyze
  (:require [gamalyzer.read.edn :refer [read-logs]]
            [gamalyzer.read.mario :refer [sample-data]]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [gamalyzer.cmp.tt.cced :refer [with-warp-window diss]]
            [gamalyzer.rsrc.data :refer [game-data find-pivots]]
            [clojure-csv.core :refer [parse-csv]]
            [clojure.java.io :refer [reader]]
            [clojure.zip :as z]
            [clojure.set :refer [union intersection]]
            [clojure.string :refer [split]]
            [clojure.core.matrix :refer [set-current-implementation mutable
                                         slices to-nested-vectors new-array
                                         mget mset! shape dimension-count
                                         submatrix to-nested-vectors get-row]]
            [clojure.core.matrix.stats :refer [mean sd]])
  (:import [com.apporiented.algorithm.clustering DefaultClusteringAlgorithm Cluster AverageLinkageStrategy]
           [java.lang Math]))

(set-current-implementation :vectorz)

(defn- count-classified [k models pivot-sets]
  (count
   (filter #(= (clojure.set/intersection (into (hash-set) (take k %)) models)
               models)
           pivot-sets)))

(defn- count-correct-classifications [n noise]
  (println "count" (* noise 0.1))
  (time (let [logs (map (fn [_] (game-data :synthetic noise))
                        (range 0 n))
              five-pivots (map #(map :label
                                     (first (find-pivots 5
                                                         (:traces %)
                                                         (:domains %))))
                               logs)]
          [(count-classified 3 #{:a :b :c} five-pivots)
           (count-classified 4 #{:a :b :c} five-pivots)
           (count-classified 5 #{:a :b :c} five-pivots)])))

;(time [(count-correct-classifications 10 0)
;       (count-correct-classifications 10 2)
;       (count-correct-classifications 10 4)])

(defn- h-labels- []
  (with-open [r (reader "resources/traces/ortega_shaker/taggedPlayers.csv")]
    (reduce (fn [m [_ pers lev clus]]
              (assoc-in m [(read-string lev) pers] (read-string clus)))
            (hash-map)
            (rest (parse-csv r)))))

(defonce h-labels (h-labels-))

(defonce mario-trace-groups
  (map (fn [lev] (sample-data lev))
       (range 0 40)))

(defn h-labels->memberships [labels-by-level]
  (map (fn [[level labels]]
         (map (fn [[_ members]]
                (into (hash-set) (map (fn [[nom label]] [nom level])
                                      members)))
              (group-by second labels)))
       (sort labels-by-level)))

(h-labels->memberships h-labels)

(defn all-distances [traces doms]
  (with-warp-window 30
    (let [n (count traces)
          mat (new-array [n n])]
      (doseq [i (range 0 n)
              :let [ti (nth traces i)]
              j (range 0 n)
              :let [tj (nth traces j)]]
        (mset! mat i j (diss ti tj doms)))
      mat)))

(defonce mario-matrices
  (map (fn [logs] [(map :id (:traces logs))
                   (all-distances (:traces logs) (:domains logs))])
       mario-trace-groups))

(defn cluster-matrices [matrices]
  (let [alg (DefaultClusteringAlgorithm.)
        strat (AverageLinkageStrategy.)]
    (map (fn [[ids mat]] (.performClustering alg
                                             (into-array (map double-array mat))
                                             (into-array ids)
                                             strat))
         matrices)))

(defn single-name [mario-id] (apply str (drop 7 mario-id)))
(defn clustname->names [cn]
  (set (map #(let [[_ nom lev] (split % #"_")]
               [nom (read-string lev)])
            (split cn #"&"))))

(defn mario-matrix-minus [level blacklist]
  (let [logs (nth mario-trace-groups level)
        good-logs (filter #(not (blacklist (first (clustname->names (:id %)))))
                          (:traces logs))]
    [(map :id good-logs)
     (all-distances good-logs (:domains logs))]))

(defn gather-cluster-members [clusts]
  (if (set? clusts)
    (set (map (fn [clust]
           (let [kids (.getChildren clust)
                 name (.getName clust)]
             (if (empty? kids)
               (clustname->names name)
               (set (mapcat gather-cluster-members kids)))))
         clusts))
    (clustname->names (.getName clusts))))

(defn clusters->memberships- [k clusts]
  (if (< (count clusts) k)
    (let [biggest (apply max-key #(.getTotalDistance %) clusts)
          minus-biggest (disj clusts biggest)
          next-clusts (apply conj minus-biggest (.getChildren biggest))]
      (clusters->memberships- k next-clusts))
    (gather-cluster-members clusts)))

(defn clusters->memberships [k clust]
  (clusters->memberships- k #{clust}))

(defn sum [fun coll]
  (reduce #(+ %1 (fun %2)) 0 coll))

(defn log [n] (/ (Math/log n) (Math/log 2)))

(defn fac [n]
  (reduce * (range 1 (inc n))))

(defn trace [v] (println v) v)

(defn square [n] (* n n))

(defn ari [us vs]
  (let [R (count us)
        C (count vs)
        _ (identity [us vs])
        intersections-uv (vec (map (fn [u] (vec (map (fn [v] (count (intersection u v))) vs))) us))
        intersections-vu (vec (map (fn [v] (vec (map (fn [u] (count (intersection v u))) us))) vs))
        as (vec (map #(reduce + 0 %) intersections-uv))
        bs (vec (map #(reduce + 0 %) intersections-vu))
        N (reduce + 0 as)
        nsq (* N N)
        nijsq (sum (fn [i]
                     (sum (fn [j]
                            (square (nth (nth intersections-uv i) j)))
                          (range 0 C)))
                   (range 0 R))
        asq (sum #(square (nth as %)) (range 0 R))
        bsq (sum #(square (nth bs %)) (range 0 C))
        n11 (/ (sum (fn [i] (sum (fn [j]
                                   (let [nij (nth (nth intersections-uv i) j)]
                                     (* nij (dec nij))))
                                 (range 0 C)))
                    (range 0 R))
               2)
        n00 (/ (+ nsq
                  nijsq
                  (- (+ asq
                        bsq)))
               2)
        n01 (/ (- bsq nijsq) 2)
        n10 (/ (- asq nijsq) 2)]
    (if (== n00 n10 n01 0)
      1.0
      (float (/ (* 2 (- (* n00 n11) (* n01 n10)))
                (+ (* (+ n00 n01) (+ n01 n11)) (* (+ n00 n10) (+ n10 n11))))))))

(defn ami [us vs]
  ; us and vs are both vecs of sets
  (let [R (count us)
        C (count vs)
        intersections-uv (vec (map (fn [u] (vec (map (fn [v] (count (intersection u v))) vs))) us))
        intersections-vu (vec (map (fn [v] (vec (map (fn [u] (count (intersection v u))) us))) vs))
        as (vec (map #(reduce + 0 %) intersections-uv))
        bs (vec (map #(reduce + 0 %) intersections-vu))
        N (reduce + 0 as)
        hu (- (sum (fn [ai]
                     (let [norm-ai (/ ai N)]
                       (* norm-ai (log norm-ai))))
                   as))
        hv (- (sum (fn [bi]
                     (let [norm-bi (/ bi N)]
                       (* norm-bi (log norm-bi))))
                   bs))
        nsq (* N N)
        iuv (sum (fn [i]
                   (let [ai (nth as i)]
                     (sum (fn [j]
                            (let [bj (nth bs j)
                                  nij (nth (nth intersections-uv i) j)
                                  norm-n (/ nij N)]
                              (if (== norm-n 0)
                                0
                                (* norm-n (log (/ norm-n (/ (* ai bj) nsq)))))))
                          (range 0 C))))
                 (range 0 R))
        eiuv (sum (fn [i]
                    (let [ai (nth as i)]
                      (sum (fn [j]
                             (let [bj (nth bs j)]
                               (sum (fn [nij]
                                      (if (== nij 0)
                                        0
                                        (* (/ nij N)
                                           (log (/ (* N nij) (* ai bj)))
                                           (/ (* (fac ai) (fac bj) (fac (- N ai)) (fac (- N bj)))
                                              (* (fac N) (fac nij) (fac (- ai nij)) (fac (- bj nij)) (fac (+ (- N ai bj) nij)))))))
                                    (range (max 0 (- (+ ai bj) N)) (min ai bj)))))
                           (range 0 C))))
                  (range 0 R))
        ami-max (if (== (max hu hv) 0) 1.0 (/ (- iuv eiuv) (- (max hu hv) eiuv)))]
    {:us us :vs vs
     :ns intersections-uv
     :as as :bs bs
     :N N
     :HU hu
     :HV hv
     :IUV iuv
     :EIUV eiuv
     :AMI_max ami-max
     :q (- 1 ami-max)}))

(defn dropped-ids [level]
  (let [traces (:traces (nth mario-trace-groups level))
        inputs (map :inputs traces)
        mean-length (mean (map count inputs))
        sd-length (sd (map count inputs))
        sd3 (* 1 sd-length)
        dropped-ids (map #(first (clustname->names (:id %)))
                         (filter (fn [trace]
                                   (not (<= (- mean-length sd3)
                                            (count (:inputs trace))
                                            (+ mean-length sd3))))
                                 traces))]
    (into (hash-set) dropped-ids)))

(defn validate-similarity [level drop-os]
  (let [mlev0 (vec (nth (h-labels->memberships h-labels) level))
        outliers (if drop-os (dropped-ids level) #{})
        mlev (filterv (complement empty?) (map (fn [s] (apply disj s outliers)) mlev0))
        clev-C (first (cluster-matrices [(mario-matrix-minus level outliers)]))
        clev (vec (clusters->memberships (count mlev) clev-C))
        tab (ami clev mlev)
        ari (ari clev mlev)]
    [level (:AMI_max tab) ari]))

(defn third [c] (nth c 2))

(defn not-weird? [[_ ami _]] (< ami 1.0))
;(defn not-weird? [[_ ami _]] true)

(let [similarities (filter not-weird? (map #(validate-similarity % false) (range 0 40)))
      ol-similarities (filter not-weird? (map #(validate-similarity % true) (range 0 40)))]
  (println (clojure.string/join \newline similarities))
  (println (clojure.string/join \newline ol-similarities))
  [[[(mean (map second similarities)) (sd (map second similarities))]
    [(mean (map third similarities)) (sd (map third similarities))]]
   [[(mean (map second ol-similarities)) (sd (map second ol-similarities))]
    [(mean (map third ol-similarities)) (sd (map third ol-similarities))]]])


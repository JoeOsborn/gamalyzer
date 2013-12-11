(ns gamalyzer.data.analyze
  (:require [gamalyzer.read.edn :refer [read-logs]]
            [gamalyzer.read.mario :refer [sample-data]]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [gamalyzer.cmp.tt.cced :refer [with-warp-window diss]]
            [gamalyzer.rsrc.data :refer [game-data find-pivots]]
            [clojure-csv.core :refer [parse-csv]]
            [clojure.java.io :refer [reader]]
            [clojure.zip :as z]
            [clojure.string :refer [split]]
            [clojure.core.matrix :refer [set-current-implementation mutable
                                         slices to-nested-vectors new-array
                                         mget mset! shape square dimension-count
                                         submatrix to-nested-vectors get-row]])
  (:import [com.apporiented.algorithm.clustering DefaultClusteringAlgorithm Cluster AverageLinkageStrategy]))

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

(defn- h-labels- []
  (with-open [r (reader "resources/traces/ortega_shaker/taggedPlayers.csv")]
    (reduce (fn [m [_ pers lev clus]]
              (assoc-in m [(read-string lev) pers] (read-string clus)))
            (hash-map)
            (rest (parse-csv r)))))

(defonce h-labels (h-labels-))
h-labels

(defonce mario-trace-groups
  (map (fn [lev] (sample-data lev))
       (range 0 40)))

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

(def clev0 (second (cluster-matrices mario-matrices)))

(defn single-name [mario-id] (apply str (drop 7 mario-id)))
(defn clustname->names [cn]
  (set (map #(vec (rest (split % #"_"))) (split cn #"&"))))

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
    (do
      (println "gathering up members " clusts)
      (gather-cluster-members clusts))))

(defn clusters->memberships [k clust]
  (clusters->memberships- k (set (.getChildren clust))))

(clusters->memberships 4 clev0)

(defn print-cluster [clust indent]
  (dotimes [_ indent] (print "  "))
  (print (.getName clust) " (" (.getTotalDistance clust) ") {")
  (newline)
  (doseq [c (.getChildren clust)]
    (print-cluster c (inc indent)))
  (dotimes [_ indent] (print "  "))
  (print "}")
  (newline))

(cluster->vecs clev0)

(print-cluster clev0 0)

(defn contingency-table [u v]
  ; u and v are both sets of sets
  (let [us (seq u)
        vs (seq v)
        intersections-uv (vector (map (fn [u] (vector (map (fn [v] (count (intersection u v))) vs))) us))
        intersections-vu (map (fn [v] (vector (map (fn [u] (count (intersection v u))) us))) vs)
        usums (vector (map #(reduce + 0 %) intersections-uv))
        vsums (vector (map #(reduce + 0 %) intersections-vu))
        N (reduce + 0 usums)]
    {:us us :vs vs :ns intersections-uv :as usums :bs vsums :N N}))

(time [(count-correct-classifications 1000 0)
       (count-correct-classifications 1000 2)
       (count-correct-classifications 1000 4)])

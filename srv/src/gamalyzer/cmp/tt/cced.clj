(ns gamalyzer.cmp.tt.cced
  (:require [gamalyzer.cmp.ii :as ii]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.edn :refer [read-logs]]
            [clojure.core.matrix :refer [new-matrix mget mset! set-current-implementation
                                         shape]])
  (:import [java.lang Double]))

(set-current-implementation :vectorz)
;;;; Continuous editing distance (dynamic programming implementation)
;;;; after Chhieng & Wong, "Adaptive Distance Measurement for Time Series Databases"
;;;; We don't use constraints because we want an exact edit distance, and since
;;;; these aren't time series we don't care about preventing weird cases like
;;;; "one sequence prefixed by another sequence".

(def del-cost 1.0)
(def ins-cost 1.0)

(defn- init-matrix [m n]
  (let [mat (new-matrix m n)]
    (mset! mat 0 0 0.0)
    (doall
     (for [x (range 1 m)]
       (mset! mat x 0 (+ (mget mat (dec x) 0) del-cost))))
    (doall
     (for [y (range 1 n)]
       (mset! mat 0 y (+ (mget mat 0 (dec y)) ins-cost))))
    mat))

(defn preds [x y]
  (let [px (dec x) py (dec y)]
    (cond
     (and (> x 0) (> y 0)) [[px py] [px y] [x py]]
     (> x 0) [[px y]]
     (> y 0) [[x py]]
     true [])))

(defn- norm-score ^double [s l1 l2]
  (if (and (== l1 0.0) (== l2 0.0)) s (/ s (+ l1 l2))))

(defn best-path [mat]
  (let [[ml nl] (shape mat)
        m (dec ml) n (dec nl)]
    (loop [x m, y n, path (list [m n (norm-score (mget mat m n) m n)])]
      (if (and (== x 0.0) (== y 0.0))
        path
        (let [[mx my] (apply min-key #(mget mat (first %) (second %)) (preds x y))]
          (recur mx my
                 (conj path [mx my
                             (norm-score (mget mat mx my) mx my)
                             (if (and (= mx (dec x)) (= my (dec y))) :diag (if (= mx x) :vert :hori))])))))))

(defn distance [s1 s2 doms]
  (let [m (count s1)
        n (count s2)
        max-dist (+ m n)
        mat (init-matrix (inc m) (inc n))]
    (doall
     (for [x (range 0 m)
           y (range 0 n)
           :let [i1 (get s1 x)
                 i2 (get s2 y)
                 nx (inc x)
                 ny (inc y)]]
       (let [i-dist (ii/diss i1 i2 doms)
             best-dist (min
                        (+ i-dist (mget mat x y))
                        (+ del-cost (mget mat x ny))
                        (+ ins-cost (mget mat nx y)))]
         (mset! mat
                nx ny
                best-dist))))
    [(mget mat m n) (best-path mat)]))

(defn diss [s1 s2 doms]
  (if (= (:uuid s1) (:uuid s2))
    0
    (let [t1 (:trace s1)
          t2 (:trace s2)
          l1 (count t1)
          l2 (count t2)
          [dist path] (distance t1 t2 doms)
          max-dist (+ l1 l2)]
      (/ dist max-dist))))

(defn tst [lim]
  (time (doall (let [logs (read-logs "/Users/jcosborn/Projects/game/xsb/logs/log.i.trace"
                                     lim
                                     (hash-set :system :random)
                                     nil)
                     vs (:traces logs)
                     doms (:domains logs)
                     vss (vec (keys vs))]
                 (for [x (range 0 lim)
                       y (range (inc x) lim)
                       :let [id1 (get vss x), id2 (get vss y)]]
                   [x y
                    (double (diss {:uuid id1, :trace (get vs id1)}
                                  {:uuid id2, :trace (get vs id2)}
                                  doms))])))))

(tst 10)

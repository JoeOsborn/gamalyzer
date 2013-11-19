(ns gamalyzer.cmp.tt.cced
  (:require [gamalyzer.cmp.ii :as ii]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.synth :refer [read-logs]]
            [clojure.core.matrix :refer [new-matrix mget mset! fill!
                                         set-current-implementation
                                         shape]])
  (:import [java.lang Double]))

(set-current-implementation :vectorz)
;;;; Constrained continuous editing distance (dynamic programming implementation)
;;;; after Chhieng & Wong, "Adaptive Distance Measurement for Time Series Databases"
;;;; We use a warp window to keep the comparisons relevant, knowing a priori that
;;;; all time series start at the same time.

(def warp-window 10)
(def del-cost 1.0)
(def ins-cost 1.0)

(defn- init-matrix [m n]
  (let [mat (new-matrix m n)]
    (fill! mat Double/POSITIVE_INFINITY)
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
  (cond
   (and (== l1 0.0) (== l2 0.0)) s
   true (/ s (* 2 (min (* (inc l1) ins-cost) (* (inc l2) del-cost))))))

(defn best-path [mat]
  (let [[ml nl] (shape mat)
        m (dec ml) n (dec nl)]
    (loop [x m, y n,
           path (list [m n (norm-score (mget mat m n) m n)])]
      (if (and (== x 0.0) (== y 0.0))
        path
        (let [[mx my] (apply min-key #(mget mat (first %) (second %)) (preds x y))]
          (recur mx my
                 (conj path [mx my
                             (norm-score (mget mat mx my) mx my)
                             (if (and (= mx (dec x)) (= my (dec y)))
                               :match
                               (if (= mx x)
                                 :delete
                                 :insert))])))))))

(defmacro sc-for [xv m yv n w let-forms & body]
  `(let [m# ~m n# ~n w# ~w]
     (if (>= m# n#)
       (doseq [:let [slope# (/ m# n#)]
               ~yv (range 1 (inc n#))
               :let [x-here# (* slope# ~yv)]
               ~xv (range (max 1 (inc (- x-here# w#)))
                          (min (inc m#) (dec (+ x-here# w#))))
               :let ~let-forms]
         ~@body)
       (doseq [:let [slope# (/ n# m#)]
               ~xv (range 1 (inc m#))
               :let [y-here# (* slope# ~xv)]
               ~yv (range (max 1 (inc (- y-here# w#)))
                          (min (inc n#) (dec (+ y-here# w#))))
               :let ~let-forms]
         ~@body))))

(defn get-cost [mat m n w x y] (mget mat x y))

(defn distance [s1 s2 doms]
  (let [m (count s1)
        n (count s2)
        mat (init-matrix (inc m) (inc n))]
    (when (or (== m 0) (== n 0))
      (throw (IllegalArgumentException. (str "A sequence is empty:" s1 ".v." s2))))
    (sc-for nx m ny n warp-window
            [px (dec nx)
             py (dec ny)
             i1 (get s1 px)
             i2 (get s2 py)]
            (let [i-dist-0 (ii/diss i1 i2 doms)
                  i-dist (if (< i-dist-0 1.0) i-dist-0 Double/POSITIVE_INFINITY)
                  ; del-cost/ins-cost are 0 at the edges.
                  ; this is because prefixes are fine.
                  dc (if (= ny n) 0 del-cost)
                  ic (if (= nx m) 0 ins-cost)
                  best-dist (min
                             (+ i-dist (get-cost mat m n warp-window px py))
                             (+ dc (get-cost mat m n warp-window px ny))
                             (+ ic (get-cost mat m n warp-window nx py)))]
              (mset! mat
                     nx ny
                     best-dist)))
    [(mget mat m n) (best-path mat)]))

(defn diss [s1 s2 doms]
  (if (= (:id s1) (:id s2))
    0
    (let [t1 (:inputs s1)
          t2 (:inputs s2)
          l1 (count t1)
          l2 (count t2)
          [dist path] (distance t1 t2 doms)]
      (norm-score dist l1 l2))))

(defn tst [lim]
  (time (doall
         (let [dur 25
               logs (read-logs [[:a (/ lim 3) dur {[1 [:a] [:a]] 1.0}]
                                [:b (/ lim 3) dur {[1 [:b] [:a]] 1.0}]
                                [:c (/ lim 3) dur {[1 [:a] [:b]] 1.0}]]
                               (hash-set :system :random)
                               nil)
               vs (:traces logs)
               doms (:domains logs)
               vss (vec (keys vs))]
           (for [x (range 0 lim)
                 y (range (inc x) lim)
                 :let [id1 (get vss x), id2 (get vss y)]]
             [x y
              (double (diss (get vs id1)
                            (get vs id2)
                            doms))])))))

(tst 18)

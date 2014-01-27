(ns gamalyzer.cmp.tt.cced
  (:require [gamalyzer.cmp.ii :as ii]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs ceil floor]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.core.matrix :refer [new-matrix new-vector
																				 fill fill! assign shape
                                         mget mset!]])
  (:import [java.lang Double System]))

;;;; Constrained continuous editing distance (dynamic programming implementation)
;;;; after Chhieng & Wong, "Adaptive Distance Measurement for Time Series Databases"
;;;; We use a warp window to keep the comparisons relevant, knowing a priori that
;;;; all time series start at the same time.

;;;; Cleanup notes: Types might help here.

(def ^:dynamic *warp-window* 5)

;;; Bind warp-window to the given value within terms t.
(defmacro with-warp-window [w & t]
  `(binding [*warp-window* ~w] ~@t))

(def del-cost 1.0)
(def ins-cost 1.0)
(def Infinity Double/POSITIVE_INFINITY)

;;; Initializes a matrix with a single column and row of zeros,
;;; and the remainder of cells set to Infinity.
(defn- init-matrix [m n]
  (let [mat (new-matrix :vectorz m n)]
    (fill! mat Infinity)
    (mset! mat 0 0 0.0)
    (doall
     (for [x (range 1 m)]
       (mset! mat x 0 (+ (mget mat (dec x) 0) del-cost))))
    (doall
     (for [y (range 1 n)]
       (mset! mat 0 y (+ (mget mat 0 (dec y)) ins-cost))))
    mat))

;;; Non-incomparable predecessors of a given pair in the matrix.
(defn- preds [mat x y]
  (let [v (mget mat x y)
				px (dec x)
				py (dec y)]
    (concat
     (if (and (> x 0)
							(> y 0)
							(< (abs (- v (mget mat px py))) 1.0))
			 [[px py]]
			 [])
     (if (and (> x 0)
							(<= (abs (- v (mget mat px y))) (+ ins-cost 0.01)))
			 [[px y]]
			 [])
     (if (and (> y 0)
							(<= (abs (- v (mget mat x py))) (+ del-cost 0.01)))
			 [[x py]]
			 []))))

;;; True if x and y is an origin point.
(defn- origin? [x y] (and (== x 0.0) (== y 0.0)))

;;; Normalizes a score according to the position in the matrix.
(defn- norm-score ^double [s l1 l2]
  (if (origin? l1 l2)
    s
    (/ s (+ (* l1 ins-cost)
            (* l2 del-cost)))))

;;; Returns the cheapest preceding x, y, and score.
;;; Some of its complexity is due to handling the case
;;; where we're in an incomparable part of the sequences
;;; because of warp window violations.
(defn- select-predecessor [ps mat x y]
	(if (empty? ps)
		(let [mx (if (> x y) (max (dec x) 0) x)
					my (if (>= y x) (max (dec y) 0) y)]
			[mx my (min (+ (* mx ins-cost)
										 (* my del-cost))
									(mget mat mx my))])
		(let [[mx my] (apply min-key #(mget mat (first %) (second %)) ps)]
			[mx my (mget mat mx my)])))

;;; Depending on the relationship between x/mx and y/my,
;;; classifies the move as a match, an insertion, or a deletion.
(defn- move-type [x y mx my]
	(if (and (= mx (dec x)) (= my (dec y)))
		:match
		(if (= mx x) :delete :insert)))

;;; Go backwards through the matrix to find the best path
;;; from the origin.
(defn- best-path [mat]
  (let [[ml nl] (shape mat)
        m (dec ml)
				n (dec nl)]
		(loop [x m, y n,
					 path (list [m n (norm-score (mget mat m n) m n) :end])]
			(let [ps (preds mat x y)]
				(if (origin? x y)
					path
					(let [[mx my cur-score] (select-predecessor ps mat x y)
								normalized-score (norm-score cur-score mx my)
								move (move-type x y mx my)]
						(recur mx my
									 (conj path [mx my normalized-score move]))))))))

;;; Depending on whether m or n is smaller, iterate through
;;; one by the other, with a step size equal to the ratio
;;; of the lengths constrained by the warp window.
;;; Evaulate this for its side effects only.
(defmacro sc-doseq [xv m yv n w let-forms & body]
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

;;; Any distances >= 1.0 are basically "incomparable" and thus Infinity.
(defn- input-distance [i1 i2 doms]
	(let [d (ii/distance i1 i2 doms)]
		(if (< d 1.0) d Infinity)))

;;; This is the core algorithm of CCED, the ED part.
;;; Iterate forward through the initialized matrix (according to the
;;; constraints provided by the sc-doseq macro), making (and storing)
;;; the best transition possible at each location based on the input/input
;;; dissimilarity at that location. Return the bottom right corner of the
;;; matrix along with the best path through the matrix.
(defn- edit-path [s1 s2 doms]
  (let [m (count s1)
        n (count s2)
        mat (init-matrix (inc m) (inc n))]
    (sc-doseq nx m ny n *warp-window*
							[px (dec nx)
							 py (dec ny)]
							(let [i-dist (input-distance (get s1 px)
																					 (get s2 py)
																					 doms)
										best-move (min (+ i-dist (mget mat px py))
																	 (+ del-cost (mget mat px ny))
																	 (+ ins-cost (mget mat nx py)))]
								(mset! mat nx ny best-move)))
		[(mget mat m n) (best-path mat)]))

;;; gives an overall distance measurement.
(defn distance [s1 s2 doms]
  (if (= (:id s1) (:id s2))
    0
    (let [t1 (:inputs s1)
          t2 (:inputs s2)
          [dist _path] (edit-path t1 t2 doms)]
      (norm-score dist (count t1) (count t2)))))

;;; gives a vector of distances with a length L where
;;; max{|s1|,|s2|} <= L <= |s1|+|s2|.
(defn distances [s1 s2 doms]
	(if (= (:id s1) (:id s2))
		(new-vector :vectorz (+ (count s1) (count s2)))
		(let [[_dist path] (edit-path (:inputs s1) (:inputs s2) doms)]
			(assign (new-vector :vectorz (count path))
							(map #(get % 2) path)))))

; Informal tests and usage examples.

#_(defn tst [lim]
  (time (doall
         (let [dur 25
               logs (gamalyzer.read.synth/sample-data
										 [[:a (/ lim 3) dur {[1 [:a] [:a]] 1.0}]
											[:b (/ lim 3) dur {[1 [:b] [:a]] 1.0}]
											[:c (/ lim 3) dur {[1 [:a] [:b]] 1.0}]]
										 (hash-set :system :random)
										 nil)
               vs (:traces logs)
               doms (:domains logs)]
           (for [x (range 0 lim)
                 y (range (inc x) lim)
                 :let [t1 (nth vs x), t2 (nth vs y)]]
             [x y (double (distance t1 t2 doms))])))))

#_(tst 18)

#_(defn tst-t [lim]
  (time (doall
         (let [dur 40
               logs (gamalyzer.read.synth/sample-data
										 [[:a (/ lim 4) dur {[1 [:a] [:a]] 1.0}]
											[:b (/ lim 4) (+ dur 15) {[1 [:b] [:a]] 1.0}]
											[:c (/ lim 4) dur {[1 [:a] [:b]] 1.0}]
											[:d (/ lim 4) dur {[1 [:a] [:a]] 0.5 [1 [:b] [:a]] 0.5}]]
										 (hash-set :system :random)
										 nil)
               vs (:traces logs)
               doms (:domains logs)]
           (for [x (range 0 lim)
                 y (range (inc x) lim)
                 :let [t1 (nth vs x), t2 (nth vs y)]]
             [x y (distances t1 t2 doms)])))))

#_(tst-t 10)

#_(let [vs [{:label :bc, :id "7", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:b], :player 1, :time 1} {:vals [:a], :det [:b], :player 1, :time 2} {:vals [:b], :det [:a], :player 1, :time 3} {:vals [:a], :det [:b], :player 1, :time 4} {:vals [:a], :det [:b], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:b], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:b], :player 1, :time 9}], :similar-count 34} {:label :ab, :id "5", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:a], :player 1, :time 1} {:vals [:a], :det [:a], :player 1, :time 2} {:vals [:a], :det [:a], :player 1, :time 3} {:vals [:a], :det [:a], :player 1, :time 4} {:vals [:a], :det [:a], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:a], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:a], :player 1, :time 9}], :similar-count 37}]
      doms (gamalyzer.data.input/expand-domain** vs (gamalyzer.data.input/make-domains))
      vss (map :id vs)]
  (doall (for [a vs
               b vs]
           (distances a b doms))))

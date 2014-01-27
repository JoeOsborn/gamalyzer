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

(def ^:dynamic *warp-window* 5)

(defmacro with-warp-window [w & t]
  `(binding [*warp-window* ~w] ~@t))

(def del-cost 1.0)
(def ins-cost 1.0)

(defn- init-matrix [m n]
  (let [mat (new-matrix :vectorz m n)]
    (fill! mat Double/POSITIVE_INFINITY)
    (mset! mat 0 0 0.0)
    (doall
     (for [x (range 1 m)]
       (mset! mat x 0 (+ (mget mat (dec x) 0) del-cost))))
    (doall
     (for [y (range 1 n)]
       (mset! mat 0 y (+ (mget mat 0 (dec y)) ins-cost))))
    mat))

(defn- preds [mat x y]
  (let [v (mget mat x y) px (dec x) py (dec y)]
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

(defn- norm-score ^double [s l1 l2]
  (if (and (== l1 0) (== l2 0))
    s
    (/ s (+ (* l1 ins-cost)
            (* l2 del-cost)))))

(defn- best-path [mat]
  (let [[ml nl] (shape mat)
        m (dec ml) n (dec nl)]
		(loop [x m, y n,
					 path (list [m n (norm-score (mget mat m n) m n) :end])]
			(let [ps (preds mat x y)]
				(if (and (== x 0.0) (== y 0.0))
					path
					(let [[mx my cur-score]
								(if (empty? ps)
									(let [mx (if (> x y) (max (dec x) 0) x)
												my (if (>= y x) (max (dec y) 0) y)
												cur-score (min
																	 (+ (* mx ins-cost) (* my del-cost))
																	 (mget mat mx my))]
										[mx my cur-score])
									(let [[mx my] (apply min-key #(mget mat (first %) (second %)) ps)
												cur-score (mget mat mx my)]
										[mx my cur-score]))]
						(recur mx my
									 (conj path
												 [mx
													my
													(norm-score cur-score mx my)
													(if (and (= mx (dec x)) (= my (dec y)))
														:match
														(if (= mx x)
															:delete
															:insert))]))))))))

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

(defn- get-cost [mat m n w x y] (mget mat x y))

(defn- distance [s1 s2 doms]
  (let [m (count s1)
        n (count s2)
        mat (init-matrix (inc m) (inc n))]
    (when (or (== m 0) (== n 0))
      (throw (IllegalArgumentException.
							(str "A sequence is empty:" s1 ".v." s2))))
    (sc-for nx m ny n *warp-window*
            [px (dec nx)
             py (dec ny)
             i1 (get s1 px)
             i2 (get s2 py)]
            (let [i-dist-0 (ii/diss i1 i2 doms)
                  i-dist (if (< i-dist-0 1.0)
													 i-dist-0
													 Double/POSITIVE_INFINITY)
                  ; del-cost/ins-cost are 0 at the edges.
                  ; this is because prefixes/postfixes are fine.
                  dc del-cost
                  ic ins-cost
                  best-dist (min
                             (+ i-dist (mget mat px py))
                             (+ dc (mget mat px ny))
                             (+ ic (mget mat nx py)))]
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

;;; gives a vector of distances with a length L where
;;; max{|s1|,|s2|} <= L <= |s1|+|s2|.
(defn diss-t [s1 s2 doms]
  (let [t1 (:inputs s1)
        t2 (:inputs s2)
        len (+ (count t1) (count t2))]
    (if (= (:id s1) (:id s2))
      (new-vector :vectorz (max (count s1) (count s2)))
      (let [[dist path] (distance t1 t2 doms)
            len (count path)
            fwd-path path
            vct (new-vector :vectorz (count fwd-path))]
				(assign vct (map #(get % 2) fwd-path))))))

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
             [x y (double (diss t1 t2 doms))])))))

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
             [x y (diss-t t1 t2 doms)])))))

#_(tst-t 10)

#_(let [vs [{:label :bc, :id "7", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:b], :player 1, :time 1} {:vals [:a], :det [:b], :player 1, :time 2} {:vals [:b], :det [:a], :player 1, :time 3} {:vals [:a], :det [:b], :player 1, :time 4} {:vals [:a], :det [:b], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:b], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:b], :player 1, :time 9}], :similar-count 34} {:label :ab, :id "5", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:a], :player 1, :time 1} {:vals [:a], :det [:a], :player 1, :time 2} {:vals [:a], :det [:a], :player 1, :time 3} {:vals [:a], :det [:a], :player 1, :time 4} {:vals [:a], :det [:a], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:a], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:a], :player 1, :time 9}], :similar-count 37}]
      doms (gamalyzer.data.input/expand-domain** vs (gamalyzer.data.input/make-domains))
      vss (map :id vs)]
  (doall (for [a vs
               b vs]
           (diss-t a b doms))))

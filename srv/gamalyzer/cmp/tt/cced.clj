(ns gamalyzer.cmp.tt.cced
  (:require [gamalyzer.cmp.ii :as ii]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :refer [abs ceil floor]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.synth :refer [read-logs]]
            [clojure.core.matrix :refer [new-matrix mget mset! fill fill! assign shape
                                         new-vector
                                         set-current-implementation]])
  (:import [java.lang Double System]))

(set-current-implementation :vectorz)
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

(defn preds [mat x y]
  (let [v (mget mat x y) px (dec x) py (dec y)]
    (concat
     (if (and (> x 0) (> y 0) (< (abs (- v (mget mat px py))) 1.0)) [[px py]] [])
     (if (and (> x 0) (<= (abs (- v (mget mat px y))) (+ ins-cost 0.01))) [[px y]] [])
     (if (and (> y 0) (<= (abs (- v (mget mat x py))) (+ del-cost 0.01))) [[x py]] []))))

(defn- norm-score ^double [s l1 l2]
  (if (and (== l1 0) (== l2 0))
    s
    (/ s (+ (* l1 ins-cost)
            (* l2 del-cost)))))

(defn best-path [mat]
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

(defn get-cost [mat m n w x y] (mget mat x y))

(defn distance [s1 s2 doms]
  (let [m (count s1)
        n (count s2)
        mat (init-matrix (inc m) (inc n))]
    (when (or (== m 0) (== n 0))
      (throw (IllegalArgumentException. (str "A sequence is empty:" s1 ".v." s2))))
    (sc-for nx m ny n *warp-window*
            [px (dec nx)
             py (dec ny)
             i1 (get s1 px)
             i2 (get s2 py)]
            (let [i-dist-0 (ii/diss i1 i2 doms)
                  i-dist (if (< i-dist-0 1.0) i-dist-0 Double/POSITIVE_INFINITY)
                  ; del-cost/ins-cost are 0 at the edges.
                  ; this is because prefixes are fine.
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

(defn resample-path [path length]
  (let [scale (/ (count path) length)]
    (map (fn [l]
           (let [i (* l scale)
                 lo (max 0 (floor i))
                 hi (min (dec (count path)) (ceil i))
                 lo-v (nth (nth path lo) 2)
                 hi-v (nth (nth path hi) 2)
                 hi-wt (- 1 (- hi i))
                 lo-wt (- 1 (- i lo))
                 weighted-avg (/ (+ (* hi-wt hi-v) (* lo-wt lo-v)) 2)]
             (when-not (== 2.0 (+ hi-wt lo-wt))
               (println "uh oh" lo hi lo-wt hi-wt))
             weighted-avg))
         (range 0 length))))

;;; gives a vector of distances of length |s1|.
;;; this includes the parts of the path that are insertions
;;; and matches, but not deletions, so this vector is "clocked"
;;; to s1.
(defn diss-t [s1 s2 doms]
  (let [t1 (:inputs s1)
        t2 (:inputs s2)
        len (+ (count t1) (count t2))]
    (if (= (:id s1) (:id s2))
      (new-vector (max (count s1) (count s2)))
      (let [[dist path] (distance t1 t2 doms)
            len (count path)
            fwd-path path
            ;fwd-path (resample-path path len)
            ;fwd-path (map #(get % 2) (filter #(not (= (get % 3) :delete)) path))
            vct (new-vector (count fwd-path))]
;        (when-not (== (count fwd-path) len) (println "path " fwd-path " is " (count fwd-path) ", not " (inc len)))
        ;(println (:label s1) (:label s2))
        ;(println "P:" path)
        ;(println "F:" fwd-path)
        ;(println "VEC:" (assign vct (map #(get % 2) fwd-path)))
        (assign vct (map #(get % 2) fwd-path))))))

#_(let [logs (gamalyzer.read.mario/sample-data)
      vs (:traces logs)
      doms (:domains logs)
      maria (first (filter #(.startsWith (:id %) "replay_MariaJesus_") vs))
      emil (first (filter #(.startsWith (:id %) "replay_Emil_") vs))
      keith (first (filter #(.startsWith (:id %) "replay_Keith_") vs))
      bb (ii/diss (second (:inputs maria)) (second (:inputs emil)) doms)]
  [; (map :vals (:inputs maria))
   ; (map :vals (:inputs emil))
   (:vals (second (:inputs maria))) (:vals (second (:inputs emil)))
   bb
   (diss-t maria emil doms)
   (diss-t maria keith doms)])

#_(defn tst [lim]
  (time (doall
         (let [dur 25
               logs (read-logs [[:a (/ lim 3) dur {[1 [:a] [:a]] 1.0}]
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
               logs (read-logs [[:a (/ lim 4) dur {[1 [:a] [:a]] 1.0}]
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

#_(let [vs [{:label :bc, :id "7bb331c9-0347-4704-8265-ec9d3264e750", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:b], :player 1, :time 1} {:vals [:a], :det [:b], :player 1, :time 2} {:vals [:b], :det [:a], :player 1, :time 3} {:vals [:a], :det [:b], :player 1, :time 4} {:vals [:a], :det [:b], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:b], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:b], :player 1, :time 9}], :similar-count 34} {:label :ab, :id "5cc80077-4c4c-4c27-ab62-1410180f53ee", :inputs [{:vals [:a], :det [:b], :player 1, :time 0} {:vals [:a], :det [:a], :player 1, :time 1} {:vals [:a], :det [:a], :player 1, :time 2} {:vals [:a], :det [:a], :player 1, :time 3} {:vals [:a], :det [:a], :player 1, :time 4} {:vals [:a], :det [:a], :player 1, :time 5} {:vals [:a], :det [:b], :player 1, :time 6} {:vals [:a], :det [:a], :player 1, :time 7} {:vals [:a], :det [:b], :player 1, :time 8} {:vals [:a], :det [:a], :player 1, :time 9}], :similar-count 37}]
      doms (gamalyzer.data.input/expand-domain** vs (gamalyzer.data.input/make-domains))
      vss (map :id vs)]
  (doall (for [a vs
               b vs]
           (diss-t a b doms))))

#_(let [a {:id :synthetic, :inputs (list {:time 0, :player :mario, :det (list :move), :vals (list 0 0 0 0)} {:time 1, :player :mario, :det (list :move), :vals (list 0 0 0 0)} {:time 2, :player :mario, :det (list :move), :vals (list 0 0 0 0)} {:time 3, :player :mario, :det (list :move), :vals (list 0 0 0 0)} {:time 4, :player :mario, :det (list :move), :vals (list 0 0 0 0)} {:time 5, :player :mario, :det (list :move), :vals (list 0 0 0 0)})}

      b {:id "human-ld1-lvl1.act", :inputs (list {:time 0, :player :mario, :det (list :move), :vals (list 0 0 0 1)} {:time 1, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 2, :player :mario, :det (list :move), :vals (list 3 1 0 0)} {:time 3, :player :mario, :det (list :move), :vals (list 6 0 0 0)} {:time 4, :player :mario, :det (list :move), :vals (list 6 3 0 3)} {:time 5, :player :mario, :det (list :move), :vals (list 6 0 0 2)} {:time 6, :player :mario, :det (list :move), :vals (list 4 0 0 3)} {:time 7, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 8, :player :mario, :det (list :move), :vals (list 4 1 0 2)} {:time 9, :player :mario, :det (list :move), :vals (list 6 0 0 5)} {:time 10, :player :mario, :det (list :move), :vals (list 3 1 0 1)} {:time 11, :player :mario, :det (list :move), :vals (list 6 0 0 4)} {:time 12, :player :mario, :det (list :move), :vals (list 3 1 0 2)} {:time 13, :player :mario, :det (list :move), :vals (list 1 1 0 1)} {:time 14, :player :mario, :det (list :move), :vals (list 3 2 0 0)} {:time 15, :player :mario, :det (list :move), :vals (list 2 0 0 4)} {:time 16, :player :mario, :det (list :move), :vals (list 0 0 0 2)} {:time 17, :player :mario, :det (list :move), :vals (list 3 0 0 2)} {:time 18, :player :mario, :det (list :move), :vals (list 1 0 0 2)} {:time 19, :player :mario, :det (list :move), :vals (list 2 0 0 3)} {:time 20, :player :mario, :det (list :move), :vals (list 5 0 0 3)} {:time 21, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 22, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 23, :player :mario, :det (list :move), :vals (list 6 0 0 0)} {:time 24, :player :mario, :det (list :move), :vals (list 3 1 0 2)} {:time 25, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 26, :player :mario, :det (list :move), :vals (list 3 2 0 0)} {:time 27, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 28, :player :mario, :det (list :move), :vals (list 1 3 0 0)} {:time 29, :player :mario, :det (list :move), :vals (list 2 2 0 0)} {:time 30, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 31, :player :mario, :det (list :move), :vals (list 2 1 0 0)} {:time 32, :player :mario, :det (list :move), :vals (list 4 1 0 2)} {:time 33, :player :mario, :det (list :move), :vals (list 1 3 0 0)} {:time 34, :player :mario, :det (list :move), :vals (list 4 2 0 0)} {:time 35, :player :mario, :det (list :move), :vals (list 4 4 0 0)} {:time 36, :player :mario, :det (list :move), :vals (list 5 1 0 3)} {:time 37, :player :mario, :det (list :move), :vals (list 1 0 0 1)} {:time 38, :player :mario, :det (list :move), :vals (list 0 1 0 1)} {:time 39, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 40, :player :mario, :det (list :move), :vals (list 5 1 0 2)} {:time 41, :player :mario, :det (list :move), :vals (list 4 2 0 3)} {:time 42, :player :mario, :det (list :move), :vals (list 2 1 0 2)} {:time 43, :player :mario, :det (list :move), :vals (list 3 0 0 2)} {:time 44, :player :mario, :det (list :move), :vals (list 5 0 0 5)} {:time 45, :player :mario, :det (list :move), :vals (list 2 1 0 0)} {:time 46, :player :mario, :det (list :move), :vals (list 6 2 0 2)} {:time 47, :player :mario, :det (list :move), :vals (list 0 3 0 2)} {:time 48, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 49, :player :mario, :det (list :move), :vals (list 3 0 0 0)} {:time 50, :player :mario, :det (list :move), :vals (list 1 3 0 0)} {:time 51, :player :mario, :det (list :move), :vals (list 2 1 0 0)} {:time 52, :player :mario, :det (list :move), :vals (list 6 0 0 1)} {:time 53, :player :mario, :det (list :move), :vals (list 0 1 0 1)} {:time 54, :player :mario, :det (list :move), :vals (list 1 1 0 2)} {:time 55, :player :mario, :det (list :move), :vals (list 0 0 0 2)} {:time 56, :player :mario, :det (list :move), :vals (list 0 0 0 2)} {:time 57, :player :mario, :det (list :move), :vals (list 0 0 0 3)} {:time 58, :player :mario, :det (list :move), :vals (list 0 0 0 1)} {:time 59, :player :mario, :det (list :move), :vals (list 3 0 0 3)} {:time 60, :player :mario, :det (list :move), :vals (list 1 1 0 0)} {:time 61, :player :mario, :det (list :move), :vals (list 0 2 0 0)} {:time 62, :player :mario, :det (list :move), :vals (list 1 0 0 0)} {:time 63, :player :mario, :det (list :move), :vals (list 2 1 0 1)} {:time 64, :player :mario, :det (list :move), :vals (list 3 0 0 3)} {:time 65, :player :mario, :det (list :move), :vals (list 2 0 0 2)} {:time 66, :player :mario, :det (list :move), :vals (list 0 0 0 5)} {:time 67, :player :mario, :det (list :move), :vals (list 3 0 0 2)} {:time 68, :player :mario, :det (list :move), :vals (list 0 0 0 2)} {:time 69, :player :mario, :det (list :move), :vals (list 0 0 0 4)} {:time 70, :player :mario, :det (list :move), :vals (list 2 0 0 0)} {:time 71, :player :mario, :det (list :move), :vals (list 2 0 0 3)} {:time 72, :player :mario, :det (list :move), :vals (list 1 0 0 3)} {:time 73, :player :mario, :det (list :move), :vals (list 3 0 0 1)} {:time 74, :player :mario, :det (list :move), :vals (list 6 0 0 6)} {:time 75, :player :mario, :det (list :move), :vals (list 1 0 0 2)} {:time 76, :player :mario, :det (list :move), :vals (list 1 0 0 0)} {:time 77, :player :mario, :det (list :move), :vals (list 1 0 0 4)} {:time 78, :player :mario, :det (list :move), :vals (list 2 0 0 2)} {:time 79, :player :mario, :det (list :move), :vals (list 0 0 0 2)} {:time 80, :player :mario, :det (list :move), :vals (list 1 0 0 2)} {:time 81, :player :mario, :det (list :move), :vals (list 2 1 0 3)} {:time 82, :player :mario, :det (list :move), :vals (list 0 3 0 0)} {:time 83, :player :mario, :det (list :move), :vals (list 4 1 0 0)} {:time 84, :player :mario, :det (list :move), :vals (list 4 1 0 3)} {:time 85, :player :mario, :det (list :move), :vals (list 4 3 0 1)} {:time 86, :player :mario, :det (list :move), :vals (list 1 1 0 3)} {:time 87, :player :mario, :det (list :move), :vals (list 1 0 0 3)} {:time 88, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 89, :player :mario, :det (list :move), :vals (list 5 2 0 2)} {:time 90, :player :mario, :det (list :move), :vals (list 1 2 0 1)} {:time 91, :player :mario, :det (list :move), :vals (list 4 1 0 3)} {:time 92, :player :mario, :det (list :move), :vals (list 4 0 0 2)} {:time 93, :player :mario, :det (list :move), :vals (list 2 0 0 1)} {:time 94, :player :mario, :det (list :move), :vals (list 2 0 0 1)} {:time 95, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 96, :player :mario, :det (list :move), :vals (list 6 1 0 4)} {:time 97, :player :mario, :det (list :move), :vals (list 2 2 0 1)} {:time 98, :player :mario, :det (list :move), :vals (list 1 0 0 1)} {:time 99, :player :mario, :det (list :move), :vals (list 1 0 0 1)} {:time 100, :player :mario, :det (list :move), :vals (list 2 2 0 2)} {:time 101, :player :mario, :det (list :move), :vals (list 2 0 0 1)} {:time 102, :player :mario, :det (list :move), :vals (list 5 0 0 4)} {:time 103, :player :mario, :det (list :move), :vals (list 2 3 0 1)} {:time 104, :player :mario, :det (list :move), :vals (list 3 2 0 0)} {:time 105, :player :mario, :det (list :move), :vals (list 2 3 0 0)} {:time 106, :player :mario, :det (list :move), :vals (list 6 0 0 4)} {:time 107, :player :mario, :det (list :move), :vals (list 5 1 0 0)} {:time 108, :player :mario, :det (list :move), :vals (list 2 2 0 0)} {:time 109, :player :mario, :det (list :move), :vals (list 4 1 0 3)} {:time 110, :player :mario, :det (list :move), :vals (list 2 3 0 1)} {:time 111, :player :mario, :det (list :move), :vals (list 6 0 0 4)} {:time 112, :player :mario, :det (list :move), :vals (list 3 1 0 1)} {:time 113, :player :mario, :det (list :move), :vals (list 1 2 0 0)} {:time 114, :player :mario, :det (list :move), :vals (list 6 0 0 0)} {:time 115, :player :mario, :det (list :move), :vals (list 6 0 0 2)} {:time 116, :player :mario, :det (list :move), :vals (list 1 0 0 0)})}
      doms (gamalyzer.data.input/expand-domain** [a b] (gamalyzer.data.input/make-domains))]
  (with-warp-window 20 (diss-t a b doms)))
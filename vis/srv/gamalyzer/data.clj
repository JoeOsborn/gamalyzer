(ns gamalyzer.data
  (:require [clojure.java.io :refer [file]]
						[clojure.string :refer [join split]]
						[compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]
            [gamalyzer.read.edn :as edn]
						[gamalyzer.read.synth :as synth]
						[gamalyzer.read.mario :as mario]
            [gamalyzer.cmp.tt :refer [pivot-distances]]
            [gamalyzer.cmp.tt.cced :refer [with-warp-window]]
						[gamalyzer.data.util :refer [mappify]]
            [clojure.core.matrix :refer [mutable slices to-nested-vectors new-array
                                         mget mset! shape square dimension-count
                                         submatrix to-nested-vectors get-row]])
  (:import [java.util UUID]
           [mdsj MDSJ]))

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
        kxkxd (new-array :vectorz [(count pivot-ids)
																	 (count pivot-ids)
																	 (dimension-count kxnxd 2)])]
    (doseq [pi (range 0 (count pivot-trace-indices))
            pj (range 0 (count pivot-trace-indices))
            :let [pti (nth pivot-trace-indices pj)
                  mat-kxn (get-row
													 (get-row
														(submatrix kxnxd [[pi 1] [pti 1] nil])
														0)
													 0)]]
      (assign-vec-at! kxkxd pi pj mat-kxn))
    kxkxd))

(defn min-pivot-index [col-n]
	(apply min-key
				 #(mget col-n % (dec (dimension-count col-n 1)))
				 (range 0 (dimension-count col-n 0))))

(defn tally-similars [mat pivot-ids traces]
  (reduce (fn [sims col-n]
            (let [min-pivot (min-pivot-index col-n)
                  min-pivot-id (nth pivot-ids min-pivot)
                  so-far (get sims min-pivot-id)]
              (assoc sims
								min-pivot-id (if so-far (inc so-far) 1))))
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
  (normalize-and-align
	 (vec (first (MDSJ/stressMinimization
								(into-array (map double-array kxk))
								1)))))

(defn noisify [pct kvs]
  ;reduce likelihood of each v by pct
  (let [new-norm (+ 1.0 pct)
				renorm-keyvals (fn [k]
												 [k (/ (+ (or (get kvs k) 0) (/ pct 3))
															 new-norm)])]
    (conj (into (hash-map)
                 (map (fn [[k v]]
                        [k (/ v new-norm)])
                      kvs))
					(map renorm-keyvals
							 [[1 [:a] [:a :a]]
								[1 [:a] [:a :b]]
								[1 [:a] [:b :b]]]))))

(defn synthetic-models [noise]
  (let [how-many 20
        how-long-mean 30
        how-long [(* how-long-mean 0.9) (* how-long-mean 1.1)]]
    [[:a how-many how-long (noisify noise {[1 [:a] [:a :a]] 1.0
                                           [1 [:a] [:a :b]] 0.0
                                           [1 [:a] [:b :b]] 0.0})]
     [:b how-many how-long (noisify noise {[1 [:a] [:a :a]] 0.0
                                           [1 [:a] [:a :b]] 1.0
                                           [1 [:a] [:b :b]] 0.0})]
     [:c how-many how-long (noisify noise {[1 [:a] [:a :a]] 0.0
                                           [1 [:a] [:a :b]] 0.0
                                           [1 [:a] [:b :b]] 1.0})]]))

(defn directories [path]
	(letfn
		[(gather [p]
						 (if-not p
							 []
							 (conj (gather (.getParentFile p)) p)))]
		(reverse (gather path))))

(defn split-path [path]
	;look for a file at path
	;if it's not there, keep going up through path until finding a real file
	;put everything to the right of there into a list of strings
	;keep everything to the left in a (file)
	(let [paths (directories (file (join "/" path)))
				real-path (first (filter #(.exists %) paths))
				remaining-path (join (nthrest (join "/" path)
																			(inc (count (.getPath real-path)))))]
		[real-path (split remaining-path #"/")]))

#_(split-path ["/Users" "jcosborn" "Projects" "gamalyzer" "vis" "resources" "traces" "mario" "0"])

(def default-settings
	{:reader 'gamalyzer.read.edn
	 :excess-path-separator "_"
	 :excess-path-match :sequence
	 :suffix ["edn" "trace"]})

(defn merge-settings [settings settings-file]
	(merge settings (read-string (slurp settings-file))))

(defn find-settings [path]
	(let [candidate-settings (map #(file % "gamalyzer.clj")
																(reverse (directories path)))]
		(reduce #(if (.exists %2) (merge-settings %1 %2) %1)
						default-settings
						candidate-settings)))

(defn all-trace-files [path sufs]
	(let [files (file-seq path)]
		(filter (fn [f]
							(let [fname (.getName f)]
								(and (not (= fname "gamalyzer.clj"))
										 (some #(.endsWith fname %) sufs))))
						files)))

; drop files whose components don't satisfy the matcher
; matcher is one of :sequence or :somewhere or :ignore.
; sequence: components must have, in sequence, the elements of excess-path as a prefix (as split by sep).
; somewhere: components must have all of the elements of excess-path as components (as split by sep)
; ignore: don't do anything fancy; don't filter anything out.
(defn filter-matching-files [all-files excess-path sep matcher]
	(let [sep-re (re-pattern (str "(" sep ")|\\."))]
		(filter
		 (condp = matcher
			 :ignore (fn [_] true)
			 :sequence #(.startsWith (.getName %)
															 (join sep excess-path))
			 :somewhere #(every? (apply hash-set (split (.getName %) sep-re))
													 excess-path))
		 all-files)))

(defn game-data [path]
	; look for a file at path
	; look upwards until finding a gamalyzer.clj file (or /resources/traces)
	; apply defaults from all parent directories too.
	(let [[real-path excess-path] (split-path path)
				settings (find-settings real-path)
				; Err, weird thing with the cons coming into the array
				reader-data (:reader settings)
				reader (if (symbol? reader-data)
								 reader-data
								 (first (rest reader-data)))
				reader-fn (ns-resolve reader 'read-path)
				ps (:excess-path-separator settings)
				pm (:excess-path-match settings)
				all-files (all-trace-files real-path (:suffix settings))
				; if there's any excess-path, use it to filter the files
				matching-files
					(if (empty? excess-path)
						all-files
						(filter-matching-files all-files excess-path ps pm))]
		(reader-fn matching-files real-path excess-path settings)))

(defn find-pivots [k vs doms]
  (let [n (count vs)
        k (min k n)
        [pivot-indices mat] (pivot-distances k vs doms)]
    [(map #(nth vs %) pivot-indices) mat]))

(defn process-data [logs k]
  (let [vs (:traces logs)
        doms (:domains logs)
        [pivots mat] (find-pivots k vs doms)
        pivot-ids (map :id pivots)
        msq (square mat)
        pivot-mat (kxnxd->kxkxd msq pivot-ids vs)
        similars (tally-similars msq pivot-ids vs)
        pivot-diffs-t (map to-nested-vectors (slices pivot-mat 2))]
    [(map #(or (:label %) (:id %)) pivots)
     (map #(assoc % :similar-count (or (get similars (:id %)) 0)) pivots)
     pivot-diffs-t
     (x-coords (last pivot-diffs-t))]))

(defn test-data [group k]
  (let [logs (game-data group)]
    (process-data logs k)))

(defn data [group params]
	(let [k (or (and (get params "k") (read-string (get params "k")))
							10)
				window (or (and (get params "window") (read-string (get params "window")))
									 20)]
		(with-warp-window window
			(mappify (test-data (concat ["resources" "traces"] group) k)))))

;(game-data (concat ["resources" "traces"] ["mario" "0"]))

;(data ["refraction" "5"] nil)

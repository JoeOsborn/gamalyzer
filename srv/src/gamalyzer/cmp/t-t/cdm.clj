(ns gamalyzer.cmp.t-t.cdm
  (:require [clojure.java.shell :refer [sh]]
            [clojure.core.memoize :as memo]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [fs.core :refer [tmpdir size file]]
            [gamalyzer.read.edn :refer [read-logs]]))

;;;; Universal compression-based dissimilarity as justified by Ferragina et al.:
;;;; "Compression-based classification of biological sequences and structures
;;;;  via the Universal Similarity Metric: experimental assessment"
;;;; The UCD measure is from Li et al., "The Similarity Metric".
;;;; This hard-codes the use of 7zip (PPM compression), and memoizes the
;;;; "compressed size" calculation in a LU-replacing cache.
;;;; The basic idea comes from Keogh et al.:
;;;; "Towards Parameter-Free Data Mining"
;;;; and Keogh et al. in a related paper:
;;;; "Compression-based data mining of sequential data"

(def compressed-size
  (memo/lu
   (fn [s]
     (let [d (tmpdir)
           sf (str d "s.txt")
           szf (str d "s.7z")]
       (spit (file sf) s)
       (sh "7za" "a" szf sf)
       (size szf)))
   {}
   :lu/threshold 10000))

(defn diss [s1 s2 doms]
  (let [s1sz (compressed-size s1)
        s2sz (compressed-size s2)
        s12sz (compressed-size [s1 s2])
        s21sz (compressed-size [s2 s1])
        ucd-num (max (- s12sz s1sz) (- s21sz s2sz))
        ucd-denom (max s1sz s2sz)]
    (/ ucd-num ucd-denom)))

(time (let [vs (vec (vals (:traces (read-logs "/Users/jcosborn/Projects/game/xsb/logs/log.i.trace" 3 (hash-set :system :random) nil))))]
    (map (fn [[a b]]
           (vector [a b] (double (diss (get vs a) (get vs b) nil))))
         (cartesian-product [0 1 2] [0 1 2]))))

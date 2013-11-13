(ns gamalyzer.cmp.t-t.cdm
  (:require [clojure.java.shell :refer [sh]]
            [clojure.core.cache :as cache]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [gamalyzer.read.edn :refer [read-logs]]
            [clojure.java.io :refer [writer]])
  (:import [org.apache.commons.compress.compressors.xz XZCompressorOutputStream]
           [java.io BufferedOutputStream ByteArrayOutputStream]))

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

(def C (cache/soft-cache-factory {}))

(defn compressed-size [k s]
  (if (cache/has? C k)
    (do
     (cache/hit C k)
     (cache/lookup C k))
    (let
      [out (ByteArrayOutputStream. 2048)
       cmp (XZCompressorOutputStream. out 4)
       wrt (writer cmp)]
      (.write wrt (.toString s))
      (.close wrt)
      (.close cmp)
      (let [sz (.size out)]
        (.close out)
        (cache/miss C k sz)
        sz))))

(defn diss [s1 s2 doms]
  (let [u1 (:uuid s1) t1 (:trace s1)
        u2 (:uuid s2) t2 (:trace s2)
        s1sz (compressed-size u1 t1)
        s2sz (compressed-size u2 t2)
        s12sz (compressed-size [u1 u2] [t1 t2])
        s21sz (compressed-size [u2 u1] [t2 t1])
        ucd-num (max (- s12sz s1sz) (- s21sz s2sz))
        ucd-denom (max s1sz s2sz)]
    (/ ucd-num ucd-denom)))

(time (doall (let [vs (:traces (read-logs "/Users/jcosborn/Projects/game/xsb/logs/log.i.trace" 10 (hash-set :system :random) nil))
            vss (vec (keys vs))]
    (map (fn [[a b]]
           (vector [a b] (double (diss {:uuid (get vss a), :trace (get vs (get vss a))} {:uuid (get vss b), :trace (get vs (get vss b))} nil))))
         (cartesian-product (range 0 10) (range 0 10))))))

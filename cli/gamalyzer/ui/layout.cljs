(ns gamalyzer.ui.layout
  (:require [strokes :refer [d3]]
            [gamalyzer.ui.util :refer [clip]]))

(def link-threshold 0.8)
(def link-strength 0.005) ;(fn [l] (* (- 1 (link-distance l)) 0.01)))
(defn link-distance [l] (.-distance l))
(def iterations 100)

(defn make-nodes [init-xs slices node-index]
  (let [ar (object-array (* (count slices) (count init-xs)))]
    (doseq [t (range 0 (count slices))
            i (range 0 (count init-xs))
            :let [xv (nth init-xs i)
                  obj (js-obj "x" xv "t" t "trace" i)
                  idx (node-index i t)]]
      (aset ar idx obj))
    ar))

(defn intra-layer-links [init-xs slices node-index]
  (let [ar (array)]
    (doseq [t (range 0 (count slices))
            :let [layer (nth slices (max 1 t))]
            si (range 0 (count layer))
            :let [s1l (nth layer si)]
            sj (range 0 (count s1l))
            :when (not (= si sj))
            :let [sqdist (nth s1l sj)
                  dist (.sqrt js/Math sqdist)]
            :when (< dist link-threshold)
            :let [obj (js-obj "source" (node-index si t)
                              "target" (node-index sj t)
                              "distance" (identity dist))
                  idx (count ar)]]
      (aset ar idx obj))
    ar))

(defn layout-xs [init-xs slices]
  (let [layout (.. (d3.layout.force)
                   (size [1 (count slices)])
                   (gravity 0)
                   (charge 0)
                   (linkStrength link-strength))
        node-index (fn [s t] (+ s (* t (count init-xs))))
        lnodes-js (make-nodes init-xs slices node-index)
        llinks-layers-js (intra-layer-links init-xs slices node-index)]
    (.on layout "tick" (fn []
                         (doseq [n lnodes-js]
                           (set! (.-y n) (.-t n))
                           (set! (.-x n) (clip 0 (.-x n) 1)))))
    (.nodes layout lnodes-js)
    (.links layout llinks-layers-js)
    (.linkDistance layout link-distance)
    (.start layout)
    (dotimes [_ iterations] (.tick layout))
    (.stop layout)
    (partition (count init-xs) (map #(.-x %) lnodes-js))))
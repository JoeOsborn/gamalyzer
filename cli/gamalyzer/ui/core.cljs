(ns gamalyzer.ui.core
  (:require [strokes :refer [d3]]))

(strokes/bootstrap)

(defn log [& stuff]
  (. js/console (log (apply str stuff)))
  (last stuff))

(def width 400)
(def height 400)
(when-not svg (def svg (.. d3 (select "body") (append "svg")
                           (attr {:width width :height height}))))

(def x (.. (d3.scale.linear) (domain [-1 1]) (range [0 width])))
(def y (.. (d3.scale.linear) (domain [0 1]) (range [10 height])))

(def all-symbols (vec d3.svg.symbolTypes))
(defn next-symbol [n]
  (nth (cycle all-symbols) n))

(when-not symbols (def symbols {}))
(defn pick-symbol [det]
  (when-not (get symbols det)
    (set! symbols (assoc symbols det (next-symbol (count symbols)))))
  (get symbols det))

(defn assign-symbol [inp]
  (let [det (:det inp)
        sym (pick-symbol det)]
    (.. (d3.svg.symbol) (type sym))))

(defn add-layers! [traces xs-per-layer]
  (.. y (domain [0 (count xs-per-layer)]))
  (let [trace-layers
         (.. svg
             (selectAll ".trace")
             (data (vec traces))
             (enter)
             (append "svg:g")
             (attr "class" "trace")
             (attr "data" (fn [d i] (nth (last xs-per-layer) i))))
        each-trace
         (.. trace-layers
             (selectAll ".input")
             (attr "class" "input")
             (data (fn [d i] (:inputs d)))
             (enter)
             (append "svg:path")
             (attr "class" "input")
             (attr "data" (fn [d] (str (pick-symbol (:det d)) d)))
             (attr
              "transform"
              (fn [d i j]
                (str "translate("
                     (x (nth (nth xs-per-layer i) j))
                     ","
                     (y i)
                     ")")))
             (attr "d" (.type (d3.svg.symbol) (fn [d] (pick-symbol (:det d))))))]
    each-trace))


(strokes/fetch-edn
 "data"
 (fn [err root]
   (let [pivots (nth root 1)
         ;pivots are the traces (lists of data points)
         slices (nth root 2)
         ;slices are the dissimilarity matrices
         pivot-xs (nth root 3)]
         ;pivot-xs are the x coordinates
     (add-layers! pivots (repeat (count slices) pivot-xs))

     )))

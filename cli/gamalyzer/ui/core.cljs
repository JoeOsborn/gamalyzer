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

(def input-line (d3.svg.line))

(defn input-positions [xs-per-layer i]
  (map-indexed (fn [j layer] [(x (nth layer i)) (y j)]) xs-per-layer))

(def stroke-width (.. (d3.scale.linear) (domain [0 1]) (range [0 5])))

(when-not val-colors (def val-colors {}))
(when-not cur-hue (def cur-hue 0))
(defn pick-color [values]
  (when-not (get val-colors values)
    (set! cur-hue (+ cur-hue 144))
    (set! val-colors
          (assoc val-colors
            values
            (d3.hsl cur-hue 0.8 0.8))))
  (.toString (get val-colors values)))

(defn add-layers! [traces xs-per-layer]
  (.. y (domain [0 (count xs-per-layer)]))
  (.. stroke-width (domain [0 (apply max (map :similar-count traces))]))
  (let [traces-with-positions
         (map-indexed (fn [i t]
                        (assoc t
                          :inputs
                          (map-indexed (fn [j inp]
                                 (let [ix (nth (nth xs-per-layer j) i)]
                                   (assoc inp :position [(x ix) (y j)])))
                               (:inputs t))))
                      traces)
        trace-layers
         (.. svg
             (selectAll ".trace")
             (data (vec traces-with-positions))
             (enter)
             (append "svg:g")
             (attr "class" "trace"))
        each-line
          (.. trace-layers
              ;;TODO: figure out how to get these to sit in the "g"s properly.
              (selectAll ".trace_line")
              (data (fn [d i] [d]))
              (enter)
              (append "svg:path")
              (attr "class" "trace_line")
              (attr "stroke" "currentColor")
              (attr "fill" "currentColor")
              (attr "color" "gray")
              (attr "stroke-width" (fn [d i] (stroke-width (:similar-count d))))
              (attr "d" (fn [d] (input-line (map :position (:inputs d))))))
        each-trace
         (.. svg
             (selectAll ".trace")
             (selectAll ".input")
             (data (fn [d i] (:inputs d)))
             (enter)
             (append "svg:path")
             (attr "class" "input")
             (attr "data" identity)
             (attr "stroke" "currentColor")
             (attr "fill" "currentColor")
             (attr "color" (fn [d] (pick-color (:vals d))))
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

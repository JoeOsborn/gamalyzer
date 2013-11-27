(ns gamalyzer.ui.core
  (:require [strokes :refer [d3]]))

(strokes/bootstrap)

(defn log [& stuff]
  (. js/console (log (apply str stuff)))
  (last stuff))

(def width 800)
(def height 800)
(when-not svg (def svg (.. d3 (select "body") (append "svg")
                           (attr {:width width :height height}))))

(def x (.. (d3.scale.linear) (domain [0 1]) (range [10 (- width 10)])))
(def y (.. (d3.scale.linear) (domain [0 1]) (range [(- height 10) 10])))

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

(def input-line (.interpolate (d3.svg.line) "linear"))

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

(defn trace-id [t] (get t :id))

(defn add-layers! [traces xs-per-layer]
  (let [traces-with-positions
        (map-indexed (fn [i t]
                       (assoc t
                         :inputs
                         (map-indexed (fn [j inp]
                                        (let [ix (nth (nth xs-per-layer j) i)]
                                          (assoc inp :position [(x ix) (y j)])))
                                      (:inputs t))))
                     traces)
        trace-layers (.. svg
                         (selectAll ".trace")
                         (data (vec traces-with-positions) trace-id))
        _ (.. trace-layers
              (enter)
              (append "svg:g")
              (attr "class" "trace"))
        each-line (.. trace-layers
                      (selectAll ".trace_line")
                      (data (fn [d i] [d])))
        _ (.. each-line
              (enter)
              (append "svg:path")
              (attr "class" "trace_line")
              (attr "stroke" "currentColor")
              (attr "fill" "none")
              (attr "color" "gray")
              (attr "stroke-width" (fn [d i] (stroke-width (:similar-count d))))
              (attr "d" (fn [d] (input-line (map :position (:inputs d))))))
        each-trace (.. trace-layers
                       (selectAll ".input")
                       (data (fn [d i] (:inputs d))))
        _ (.. each-trace
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

(defn pairs [s] (mapcat (fn [l] (map (fn [r] [l r]) s)) s))
(defn clip [l m h] (cond (< m l) l (> m h) h true m))

(defn layout-xs [init-xs slices]
  (let [;_ (log slices)
        layout (.. (d3.layout.force)
                   (size [1 (count slices)])
                   (gravity 0)
                   (charge 0)
                   (linkStrength 0.0001))
        lnodes (mapcat (fn [t] (map-indexed (fn [i xv] {:x xv :t t :trace i})
                                            init-xs))
                       (range 0 (count slices)))
        node-index (fn [s t] (+ s (* t (count init-xs))))
;        inter-layer-distance 1
;        llinks-traces (mapcat (fn [si] (map (fn [t] {:source (node-index si (dec t))
;                                                     :target (node-index si t)
;                                                     :distance (x inter-layer-distance)})
;                                            (range 1 (count slices))))
;                              (range 0 (count init-xs)))
        llinks-layers (mapcat (fn [[si sj]] (if (== si sj)
                                              []
                                              (map (fn [t]
                                              ;       (when-not (== (get-in slices [(if (= t 0) 1 t) si sj])
                                              ;                     (get-in slices [(if (= t 0) 1 t) sj si]))
                                              ;         (log "neq in slices " t " " si " " sj " = " (nth slices t) "----" (nth (nth slices t) si) "----" (nth (nth (nth slices t) si) sj) " vs " (nth (nth slices t) sj) "----" (nth (nth (nth slices t) sj) si)))
                                                     {:source (node-index si t)
                                                      :target (node-index sj t)
                                                      :distance (/ (+ (get-in slices [(if (= t 0) 1 t) si sj])
                                                                      (get-in slices [(if (= t 0) 1 t) sj si])) 2)})
                                                   (range 0 (count slices)))))
                              (pairs (range 0 (count init-xs))))
        ;_ (log llinks-layers)
        lnodes-js (clj->js lnodes)
        llinks-js (clj->js llinks-layers)]
;        llinks-js (clj->js (concat llinks-traces llinks-layers))]
    (.on layout "tick" (fn []
                         (doseq [n lnodes-js]
                           (set! (.-y n) (.-t n))
                           (set! (.-x n) (clip 0 (.-x n) 1)))))
    (.nodes layout lnodes-js)
    (.links layout llinks-js)
    (.start layout)
    (dotimes [_ 100] (.tick layout))
    (.stop layout)
    (partition (count init-xs) (map #(get % "x") (js->clj lnodes-js)))))

(strokes/fetch-edn
 "data"
 (fn [err root]
   (let [pivots (nth root 1)
         ;pivots are the traces (lists of data points)
         slices (vec (nth root 2))
         ;slices are the dissimilarity matrices
         init-xs (nth root 3)]
         ;init-xs are the x coordinates
     (.. y (domain [0 (count slices)]))
     (.. stroke-width (domain [0 (apply max (map :similar-count pivots))]))
     (add-layers! pivots (layout-xs init-xs slices))
     )))

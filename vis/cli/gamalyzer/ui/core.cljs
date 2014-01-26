(ns gamalyzer.ui.core
  (:require [strokes :refer [d3]]
            [gamalyzer.ui.colors :refer [pick-color]]
            [gamalyzer.ui.util :refer [log
                                       vectorize median-element pairs
                                       get-by get-nearest
                                       clip
                                       make-slider!]]
            [gamalyzer.ui.symbols :refer [pick-symbol]]
            [gamalyzer.ui.layout :refer [layout-xs
						                             link-threshold link-strength iterations]]))

(strokes/bootstrap)

(def mode :mario)

(def width 400)
(def height 400)
(def x (.. (d3.scale.linear) (domain [0 1]) (range [10 (- width 10)])))
(def y (.. (d3.scale.linear) (domain [0 1]) (range [(- height 10) 10])))
(defn y->t [v] (max 0 (.round js/Math (.invert y v))))

(def level (mode {:mario 0 :refraction 5 :synthetic 0}))
(def pivot-count 10)
(def warp-window 20)
(def fetched-data nil)

(declare kick!)

(defn reload-data! []
  (strokes/fetch-edn (str (name mode) "?level=" level "&k=" pivot-count "&window=" warp-window)
                     (fn [err root]
                       (log "load" err root)
                       (set! fetched-data root)
                       (kick! fetched-data))))

(make-slider! "level"
              (mode {:mario 0 :refraction 3 :synthetic 0})
              level
              (mode {:mario 39 :refraction 5 :synthetic 4})
              1
              (fn [n]
                (set! level n)
                (reload-data!)))
(make-slider! "pivot-count" 0 pivot-count 20 1
              (fn [n]
                (set! pivot-count n)
                (reload-data!)))
(make-slider! "warp-window" 0 warp-window 100 1
              (fn [w]
                (set! warp-window w)
                (reload-data!)))

(make-slider! "width" 200 width 2000 100 (fn [w] (set! width w) (kick! fetched-data)))
(make-slider! "height" 100 height 2000 50 (fn [h] (set! height h) (kick! fetched-data)))

(make-slider! "link-threshold" 0.1 link-threshold 1.0 0.05 (fn [t] (set! link-threshold t) (kick! fetched-data)))
(make-slider! "link-strength" 0.0 link-strength 0.005 0.0001 (fn [t] (set! link-strength t) (kick! fetched-data)))
(make-slider! "iterations" 0 iterations 100 20 (fn [i] (set! iterations i) (kick! fetched-data)))

(.. d3
	(select "body")
	(append "svg")
	(attr {:id "svg" :width width :height height}))
(def svg (.select d3 "body > svg"))
(.attr svg {:width width :height height})

(def info (.. d3
              (select "body")
              (append "div")
              (attr "id" "info")
              (style "opacity" 0)))
(.append info "table")

(defn get-nearest-position [inputs i]
  (if-let [inp (second (get-nearest :position inputs i))]
    (:position inp)
    (double-array 0 0)))

(defn pad-times [traces min-t max-t]
  (map (fn [trace]
         (let [inputs (:inputs trace)
               id (:id trace)]
           (reverse
            (sort-by :time
                     (map
                      (fn [t]
                        (if-let [inp (get-by #(== (:time %) t) inputs)]
                          inp
                          {:time t
                           :dummy true
                           :id id
                           :position (get-nearest-position inputs t)}))
                      (range min-t (inc max-t)))))))
       traces))

(defn highlight-selected! [mode pred]
  (.. d3
      (selectAll ".trace")
      (classed mode #(some pred (:inputs %))))
  (.. d3
      (selectAll ".trace .input")
      (classed mode #(pred %))))

(defn selected-trace-segments [min-x min-y max-x max-y]
  (let [traces (.. svg (selectAll ".trace") (data))
        inputs (filter #(and (<= min-x (first (:position %)) max-x)
                             (<= min-y (- (second (:position %)) 10) max-y))
                       (mapcat :inputs traces))
        ids-in-x (into (hash-set) (map :id inputs))
        traces-in-x (filter #(contains? ids-in-x (:id %)) traces)]
    (when-not (empty? inputs)
      (pad-times traces-in-x
                 (apply min (map :time inputs))
                 (apply max (map :time inputs))))))

(defn fade-infobox! [opacity]
  (.. info
      (transition)
      (duration 100)
      (style "opacity" opacity)))

(defn clear-infobox! []
  (.. info
        (select "table")
        (selectAll "tr")
        (remove))
  (.. info
      (select "table")
      (selectAll "tfoot")
      (remove)))

(defn show-infobox-inputs! [traces-by-time]
  (.. info
      (select "table")
      (selectAll "tr")
      (data traces-by-time)
      (enter)
      (append "tr"))
  (.. info
      (select "table")
      (selectAll "tr")
      (data (fn [] traces-by-time))
      (selectAll "td")
      (data (fn [d i] d))
      (enter)
      (append "td")
      (style "background-color" (fn [d] (pick-color (:vals d) 0.8)))
      (text (fn [d] (apply str (:time d) ". "
                           (when-not (:dummy d)
                             [(:player d) " " (:det d) " " (:vals d)]))))))

(defn show-infobox-footer! [sorted-traces]
  (.. info
      (select "table")
      (append "tfoot")
      (append "tr")
      (selectAll "td")
      (data (map first sorted-traces))
      (enter)
      (append "td")
      (text (fn [d]
              (str (:id d) "(" (:similar-count d) ")")))))

(defn show-infobox! [symbols-by-trace ex ey]
  (let [sorted-traces (sort-by #(first (:position (median-element %)))
                               symbols-by-trace)]
    (clear-infobox!)
    (show-infobox-inputs! (vec (d3.transpose (vectorize sorted-traces))))
    (show-infobox-footer! sorted-traces)
    (.style info {:left (+ ex 8)
                  :top (- ey (/ (.-clientHeight (.getElementById js/document "info")) 2))})
    (fade-infobox! 0.9)))

(defn hide-infobox! [] (fade-infobox! 0))

(defn input-contained-fn [min-x min-y max-x max-y]
  (fn [input]
    (let [[x y] (:position input)]
      (and (<= min-x x max-x)
           (<= min-y y max-y)))))

(defn tip-mouse-out [d i]
  (highlight-selected! "selected-info" (fn [_] false))
  (hide-infobox!))
(defn tip-mouse-move [d i]
  (let [e d3.event
        svg-element (.getElementById js/document "svg")
        svg-off-top (.-offsetTop svg-element)
        svg-off-left (.-offsetLeft svg-element)
        ex (- (.-pageX e) svg-off-left)
        ey (- (.-pageY e) svg-off-top 10)
        radius 8.0 ;in pixels
        min-x (- ex radius)
        max-x (+ ex radius)
        min-y (- ey radius)
        max-y (+ ey radius)
        min-t (y->t max-y)
        max-t (y->t min-y)]
    (if-let [traces (selected-trace-segments min-x min-y max-x max-y)]
      (do
        (highlight-selected! "selected-info"
                             (input-contained-fn min-x min-y
                                                 max-x max-y))
        (show-infobox! traces (.-pageX e) (.-pageY e)))
      (tip-mouse-out d i))))

(.on svg "mouseover" tip-mouse-move)
(.on svg "mousemove" tip-mouse-move)
(.on svg "mouseout" tip-mouse-out)

(def input-line (.interpolate (d3.svg.line) "linear"))
(def stroke-width (.. (d3.scale.linear) (domain [0 1]) (range [0 5])))
(defn trace-id [t] (get t :id))

(defn include-positions [traces xs-per-layer]
  (map-indexed
   (fn [i t]
     (assoc t
       :inputs
       (map-indexed (fn [j inp]
                      (let [ix (nth (nth xs-per-layer j) i)]
                        (assoc inp
                          :position (array (x ix) (y j))
                          :id (:id t)
                          :similar-count (:similar-count t))))
                    (:inputs t))))
   traces))

(defn update-trace-layers! [traces-with-positions]
  (let [trace-layers (.. svg
                         (selectAll ".trace")
                         (data traces-with-positions trace-id))]
    (.. trace-layers
        (exit)
        (remove))
    (.. trace-layers
        (enter)
        (append "svg:g")
        (attr "class" "trace"))
    trace-layers))

(defn update-trace-lines! [trace-layers]
  (let [each-line (.. trace-layers
                      (selectAll ".trace_line")
                      (data (fn [d i] [d])))]
    (.. each-line
        (enter)
        (append "svg:path")
        (attr {:class "trace_line"
               :stroke "currentColor"
               :fill "none"
               :color "gray"}))
    (.. each-line
        (attr {:stroke-width
               (fn [d i] (stroke-width
                          (if (zero? (:similar-count d))
                            1
                            (:similar-count d))))
               :d
               (fn [d] (input-line (map :position
                                        (:inputs d))))}))
    each-line))

(defn update-trace-inputs! [trace-layers]
  (let [each-trace (.. trace-layers
                       (selectAll ".input")
                       (data (fn [d i] (:inputs d))))]
    (.. each-trace
              (enter)
              (append "svg:path")
              (attr {:class "input"
                     :stroke "currentColor"
                     :fill "currentColor"}))
    (.. each-trace
              (attr {:color (fn [d] (pick-color (:vals d)))
                     :transform
                     (fn [d i j]
                       (str "translate("
                            (nth (:position d) 0)
                            ","
                            (nth (:position d) 1)
                            ")"))
                     :d (.type (d3.svg.symbol)
                               (fn [d] (pick-symbol (:det d))))}))
    each-trace))

(defn update-layers! [traces xs-per-layer]
  (let [traces-with-positions (vec (include-positions traces xs-per-layer))
        trace-layers (update-trace-layers! traces-with-positions)]
    (update-trace-lines! trace-layers)
    (update-trace-inputs! trace-layers)
    trace-layers))


(def brush (.. svg (append "g") (attr "class" "brush")))

(def *custom-vis-fn* nil)

(defn custom-vis []
  (when *custom-vis-fn* (*custom-vis-fn*)))

(defn update-brush-e! []
  (let [[[min-x min-y] [max-x max-y]]
        (.extent d3.event.target)]
    (highlight-selected! "selected-brush"
                         (input-contained-fn min-x min-y
                                             max-x max-y))
    (custom-vis)))

(defn prepare-brush! []
  (.call brush
         (.. (d3.svg.brush)
             (x (.. (d3.scale.identity) (domain [0 width])))
             (y (.. (d3.scale.identity) (domain [0 height])))
             (on "brush" update-brush-e!))))

(defn kick! [root]
  (log "kick")
  (.attr svg {:width width :height height})
  (set! x (.. (d3.scale.linear)
              (domain [0 1])
              (range [10 (- width 10)])))
  (set! y (.. (d3.scale.linear)
              (domain [0 1])
              (range [(- height 10) 10])))
  (let [pivots (nth root 1)
        ;pivots are the traces (lists of data points)
        slices (vec (nth root 2))
        ;slices are the dissimilarity matrices
        init-xs (nth root 3)]
    ;init-xs are the x coordinates
    (.domain y [0 (count slices)])
    (.domain stroke-width
             [0 (apply max 10 (map :similar-count pivots))])
    (prepare-brush!)
    (let [xs (time (layout-xs init-xs slices))]
      (time (update-layers! pivots xs))))
  (custom-vis))

(if fetched-data
  (kick! fetched-data)
  (reload-data!))

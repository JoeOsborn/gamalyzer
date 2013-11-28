(ns gamalyzer.ui.core
  (:require [strokes :refer [d3]]))

(strokes/bootstrap)

(defn log [& stuff]
  (. js/console (log (apply str stuff)))
  (last stuff))
(set! *print-fn* log)

(def width 400)
(def height 400)

(when-not info
  (do
    (def info (.. d3
                  (select "body")
                  (append "div")
                  (attr "id" "info")
                  (style "opacity" 0)))
    (.append info "table")))

(defn make-slider! [id mn v mx st f]
  (when (= 0 (.size (.select d3 (str "#" id))))
    (do
      (.. d3
          (select "body")
          (append "div")
          (attr {:class "slider" :id id})
          (call (.. (d3.slider)
                    (axis true)
                    (min mn)
                    (value v)
                    (max mx)
                    (step st)
                    (on "slide" #(f %2)))))
      (.. d3
          (select (str "#" id))
          (insert "div" ":first-child")
          (attr "class" "slider-label")
          (text id)))))

(make-slider! "width" 400 width 2000 100 (fn [w] (set! width w) (kick! fetched-data)))
(make-slider! "height" 400 height 2000 100 (fn [h] (set! height h) (kick! fetched-data)))

(def link-threshold 1.0)
(def link-strength 0.0025) ;(fn [l] (* (- 1 (link-distance l)) 0.01)))
(defn link-distance [l] (.-distance l))
(def iterations 100)

(when-not svg (.. d3 (select "body") (append "svg") (attr "id" "svg")))
(def svg (.select d3 "body > svg"))
(.property svg {:width width :height height})

(defn get-by [pred s]
  (first (filter pred s)))

(defn pad-times [symbols-by-trace min-t max-t]
  (map (fn [inputs]
         (let [id (:id (first inputs))]
           (reverse (sort-by :time (map (fn [t]
                                 (or (get-by #(== (:time %) t) inputs) {:time t :dummy true :id id}))
                               (range min-t max-t))))))
       symbols-by-trace))

(defn vectorize [s]
  (if (sequential? s)
    (into [] (map vectorize s))
    s))

(defn tip-mouse-move [d i]
  (let [e d3.event
        svg-element (.getElementById js/document "svg")
        svg-off-top (.-offsetTop svg-element)
        svg-off-left (.-offsetLeft svg-element)
        ex (- (.-pageX e) svg-off-left)
        ey (- (.-pageY e) svg-off-top 10)
        traces (.. svg (selectAll ".trace") (data))
        radius 8.0 ;in pixels
        min-x (- ex radius)
        max-x (+ ex radius)
        min-t (max 0 (.round js/Math (.invert y (+ ey radius))))
        max-t (.round js/Math (.invert y (- ey radius)))
        symbols-in-t (mapcat (fn [t] (mapcat (fn [trace]
                                               (if (> (count (:inputs trace)) t)
                                                 [(assoc
                                                    (nth (:inputs trace) t)
                                                    :id (:id trace))]
                                                 []))
                                             traces))
                             (reverse (range min-t max-t)))
        symbols-in-x (filter #(<= min-x (nth (:position %) 0) max-x)
                             symbols-in-t)
        symbols-by-trace (vec (vals (group-by #(:id %) symbols-in-x)))
        min-found-t (when-not (empty? symbols-in-x) (apply min (map :time symbols-in-x)))
        max-found-t (when-not (empty? symbols-in-x) (apply max (map :time symbols-in-x)))
        padded-symbols-by-trace (when-not (or (nil? min-found-t) (nil? max-found-t)) (pad-times symbols-by-trace min-found-t max-found-t))
        traces-by-time (d3.transpose (vectorize padded-symbols-by-trace))]
    (if-not (empty? symbols-in-x)
      (do
        (.. info
            (select "table")
            (selectAll "tr")
            (remove))
        (.. info
            (select "table")
            (selectAll "tr")
            (data (vec traces-by-time))
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
            (text (fn [d]
                    (if (:dummy d)
                      (str (:time d)". ")
                      (str (:time d) ". " (:player d) " " (:det d) " " (:vals d))))))
        (.style info "left" (- (.-pageX e) 8))
        (.style info "top" (- (.-pageY e) (/ (.-clientHeight (.getElementById js/document "info")) 2)))
        (.. info
            (transition)
            (duration 100)
            (style "opacity" 0.9)))
      (tip-mouse-out d i))))
(defn tip-mouse-out [d i] nil)
;  (.. info
;      (selectAll "tr")
;      (remove))
;  (.. info
;      (transition)
;      (duration 100)
;      (style "opacity" 0)))
(.on svg "mouseover" tip-mouse-move)
(.on svg "mousemove" tip-mouse-move)
(.on svg "mouseout" tip-mouse-out)

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
(when-not cur-hue (def cur-hue 60))
(defn wrap [n l]
  (cond
   (> n l) (wrap (- n l) l)
   (< n 0) (wrap (+ n l) l)
   true n))

(defn pick-color [values]
  (when-not (get val-colors values)
    (set! cur-hue (wrap (+ cur-hue 144) 360))
    (set! val-colors
          (assoc val-colors
            values
            (d3.hsl cur-hue 0.8 0.6))))
  (.toString (get val-colors values)))

(defn trace-id [t] (get t :id))

(defn add-layers! [traces xs-per-layer]
  (let [traces-with-positions
        (map-indexed
         (fn [i t]
           (assoc t
             :inputs
             (map-indexed (fn [j inp]
                            (let [ix (nth (nth xs-per-layer j) i)]
                              (assoc inp
                                :position
                                (array (x ix) (y j)))))
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
              (attr {:class "trace_line"
                     :stroke "currentColor"
                     :fill "none"
                     :color "gray"}))
        _ (.. each-line
              (attr {:stroke-width
                     (fn [d i] (stroke-width (:similar-count d)))
                     :d
                     (fn [d] (input-line (map :position (:inputs d))))}))
        each-trace (.. trace-layers
                       (selectAll ".input")
                       (data (fn [d i] (:inputs d))))
        _ (.. each-trace
              (enter)
              (append "svg:path")
              (attr {:class "input"
                     :stroke "currentColor"
                     :fill "currentColor"}))
        _ (.. each-trace
              (attr {:color (fn [d] (pick-color (:vals d)))
                     :transform
                     (fn [d i j]
                       (str "translate("
                            (nth (:position d) 0)
                            ","
                            (nth (:position d) 1)
                            ")"))
                     :d (.type (d3.svg.symbol)
                               (fn [d] (pick-symbol (:det d))))}))]
    trace-layers))

(defn pairs [s] (mapcat (fn [l] (map (fn [r] [l r]) s)) s))
(defn clip [l m h] (cond (< m l) l (> m h) h true m))

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
  (let [;_ (log slices)
        layout (.. (d3.layout.force)
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

(defn kick! [root]
  (.property (.select d3 "svg") {:width width :height height})
  (set! x (.. (d3.scale.linear) (domain [0 1]) (range [10 (- width 10)])))
  (set! y (.. (d3.scale.linear) (domain [0 1]) (range [(- height 10) 10])))
  (let [pivots (nth root 1)
        ;pivots are the traces (lists of data points)
        slices (vec (nth root 2))
        ;slices are the dissimilarity matrices
        init-xs (nth root 3)]
    ;init-xs are the x coordinates
    (.. y (domain [0 (count slices)]))
    (.. stroke-width (domain [0 (apply max (map :similar-count pivots))]))
    (let [xs (time (layout-xs init-xs slices))]
      (time (add-layers! pivots xs)))))

(if fetched-data
  (kick! fetched-data)
  (do (strokes/fetch-edn
   "data"
   (fn [err root]
     (log "load")
     (set! fetched-data root)
     (kick! fetched-data)))))

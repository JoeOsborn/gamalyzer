(ns gamalyzer.ui.util
  (:require [strokes :refer [d3]]))

(defn log [& stuff]
  (apply println stuff))

(defn vectorize [s]
  (if (sequential? s)
    (into [] (map vectorize s))
    s))

(defn median-element [s]
  (if (empty? s)
    nil
    (nth s (.floor js/Math (/ (count s) 2)))))

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
                    (on "slide" (fn [e v] (log "now:" v) (f v))))))
      (.. d3
          (select (str "#" id))
          (insert "div" ":first-child")
          (attr "class" "slider-label")
          (text id)))))


(defn pairs [s] (mapcat (fn [l] (map (fn [r] [l r]) s)) s))
(defn clip [l m h] (cond (< m l) l (> m h) h true m))

(defn get-by [pred s]
  (first (filter pred s)))

(defn get-nearest [keyfn s i]
  (if (and (contains? s i) (keyfn (nth s i)))
    [i (nth s i)]
    (loop [off 0]
      (let [lo (- i off)
            hi (+ i off)]
        (cond
         (and (contains? s lo) (keyfn (nth s lo))) [lo (nth s lo)]
         (and (contains? s hi) (keyfn (nth s hi))) [hi (nth s hi)]
         (and (< lo 0) (>= hi (count s))) [nil nil]
         true (recur (inc off)))))))

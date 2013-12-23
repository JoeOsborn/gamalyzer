(ns gamalyzer.ui.symbols
  (:require [strokes :refer [d3]]))

(def all-symbols (vec d3.svg.symbolTypes))
(defn next-symbol [n]
  (nth (cycle all-symbols) n))

(def symbols {})
(defn pick-symbol [det]
  (when-not (get symbols det)
    (set! symbols (assoc symbols det (next-symbol (count symbols)))))
  (get symbols det))
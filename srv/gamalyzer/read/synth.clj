(ns gamalyzer.read.synth
  (:require [gamalyzer.data.input :refer [make-input make-trace make-traces make-domains expand-domain**]])
  (:import [java.util UUID]))

(defn- model-synthesize-one [model ks t]
  (let [r (rand)]
    (loop [s 0 i 0]
      (let [k (get ks i)
            [pl det vs] k
            here (get model k)
            so-far (+ s here)]
        (if (or (>= so-far r) (>= (inc i) (count ks)))
          (make-input t pl det vs)
          (recur so-far (inc i)))))))

(defn- model-synthesize [model how-many]
  (let [ks (vec (keys model))]
    (mapv #(model-synthesize-one model ks %) (range 0 how-many))))

(defn- synth-trace [k uuid how-long model]
  (let [traces (model-synthesize model how-long)]
    (assoc (make-trace (str k "_" uuid) traces)
      :label k)))

(defn- synth-traces [k how-many how-long model]
  (map (fn [so-far]
         (let [uuid (.toString (UUID/randomUUID))
               how-long-here (if (sequential? how-long) (let [[lo hi] how-long] (+ lo (* (rand) (- hi lo)))) how-long)]
           (synth-trace k uuid how-long-here model)))
       (range 0 how-many)))

(defn read-logs [models _blacklist domains]
  (let [vs (vec (mapcat #(apply synth-traces %) models))
        doms (expand-domain** vs (if (nil? domains) (make-domains) domains))]
    (make-traces vs doms)))

(read-logs [[:a 1 2 {[1 [:a] [:a]] 1.0}]
            [:b 1 2 {[1 [:b] [:a]] 1.0}]
            [:c 1 2 {[1 [:a] [:b]] 1.0}]]
           (hash-set :system :random)
           nil)

(ns gamalyzer.read.synth
  (:require [gamalyzer.data.input :refer [make-input make-domains expand-domain**]])
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

(defn- make-trace [k uuid how-long model]
  (let [traces (model-synthesize model how-long)]
    {:id uuid :inputs traces :label k}))

(defn- make-traces [k how-many how-long model]
  (reduce (fn [so-far _]
            (let [uuid (UUID/randomUUID)]
              (assoc so-far uuid (make-trace k uuid how-long model))))
          (hash-map)
          (range 0 how-many)))

(defn read-logs [models _blacklist domains]
  (let [vs (apply merge (map #(apply make-traces %) models))
        doms (expand-domain** vs (if (nil? domains) (make-domains) domains))]
    {:domains doms :traces vs}))

(read-logs [[:a 1 2 {[1 [:a] [:a]] 1.0}]
            [:b 1 2 {[1 [:b] [:a]] 1.0}]
            [:c 1 2 {[1 [:a] [:b]] 1.0}]]
           (hash-set :system :random)
           nil)

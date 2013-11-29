(ns gamalyzer.read.mario
  (:require [clojure.java.io :refer [input-stream file]]
            [gamalyzer.data.input :refer [make-input make-domains expand-domain player]]))

(defn byte->bits [i]
  (reverse (map #(bit-test i %) (range 0 8))))

(defn downsample [[lrs js ss ds] [_ _ l r d j s _]]
  [(+ lrs (if l -1 0) (if r 1 0)) (+ js (if j 1 0)) (+ ss (if s 1 0)) (+ ds (if d 1 0))])

(defn samples->input [t some-bytes]
  (make-input t :mario [:move] (reduce downsample [0 0 0 0] (map byte->bits some-bytes))))

(defn mario->gamalyzer
  ([id integers] (mario->gamalyzer id integers 24))
  ([id integers samples-per-frame]
   {:id id
    :inputs (map-indexed samples->input (partition samples-per-frame integers))}))

(defn read-log-trace [path blacklist doms]
  (with-open [f (input-stream path)]
    (let [samples-per-frame 24
          ba (byte-array samples-per-frame)]
      (loop [inputs (transient []) doms doms]
        (let [bytes-read (.read f ba)]
          (cond
           (== bytes-read -1) [{:id (.getName (.getParentFile (file path)))
                                :inputs (persistent! inputs)} doms]
           (< bytes-read samples-per-frame)
           (do
             (doseq [i (range bytes-read samples-per-frame)]
               (aset-byte ba i 0))
             (let [inp (samples->input (count inputs) ba)]
               (recur (conj! inputs inp)
                      (expand-domain inp doms))))
           true (let [inp (samples->input (count inputs) ba)]
                  (recur (conj! inputs inp)
                         (expand-domain inp doms)))))))))

(defn find-files [path suffix maybe-deep]
  ; todo: figure out how to deeply get all files in a directory with a suffix
  [])

(defn read-logs [path how-many blacklist indoms]
  (let [files (find-files path ".act" :deep)
        domains (if (nil? indoms) (make-domains) indoms)
        limit (min (count files) (if (= how-many :all) (count files) how-many))]
    (loop [ts (transient (hash-map))
           i 0
           doms domains]
      (if (< i limit)
        (if-let [[trace new-doms] (read-log-trace (nth files i) blacklist doms)]
          (recur
           (assoc! ts (:id trace) trace)
           (inc i)
           new-doms)
          {:traces (persistent! ts) :domains doms})
        {:traces (persistent! ts) :domains doms}))))

(defn sample-data []
  (let [some-files (map #(str "/Users/jcosborn/Projects/gamalyzer/resources/traces/mario/" % "/actions.act")
                        ["lazy-cig-sergeykarakovskiy"
                         "lazy-forward"
                         "lazy-forwardjump"
                         "lazy-gic-sergeykarakovskiy"
                         "lazy-human1"
                         "lazy-human2"
                         "lazy-human3"
                         "lazy-human4"
                         "lazy-human5"])
        [ts ds] (reduce (fn [[traces doms] file]
                          (let [[trace new-doms]
                                (read-log-trace file (hash-set) doms)]
                            [(assoc traces (:id trace) trace) new-doms]))
                        [{} (make-domains)]
                        some-files)]
    {:traces ts :domains ds}))

(time (sample-data))

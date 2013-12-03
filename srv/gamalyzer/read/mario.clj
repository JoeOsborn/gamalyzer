(ns gamalyzer.read.mario
  (:require [clojure.java.io :refer [input-stream reader file]]
            [clojure.string :refer [split]]
            [gamalyzer.data.input :refer [make-input make-domains expand-domain player]]))

(defn byte->bits [i]
  (reverse (map #(bit-test i %) (range 0 8))))

(defn downsample [[ss lrs js ds] [l r d j s _]]
  [(+ ss (if s 1 0))
   (+ lrs (if l -1 0) (if r 1 0))
   (+ js (if j 1 0))
   (+ ds (if d 1 0))])

(defn samples->input [t samples]
  (make-input t :mario [:move] (seq (reduce downsample [0 0 0 0] samples))))

(defn bytes->input [t some-bytes]
  (samples->input t (map #(nthrest (byte->bits) 2) some-bytes)))

(defn csv->input [t some-strings]
  (samples->input t (map #(map (fn [s] (not (zero? (read-string s)))) (split % #",")) some-strings)))

(defn mario->gamalyzer
  ([id integers] (mario->gamalyzer id integers 24))
  ([id integers samples-per-frame]
   {:id id
    :inputs (map-indexed bytes->input (partition samples-per-frame integers))}))

(defn read-input-bytes [f ba samples-per-frame t]
  (let [bytes-read (.read f ba)]
    (cond
     (== bytes-read -1) nil
     (< bytes-read samples-per-frame)
     (do
       (doseq [i (range bytes-read samples-per-frame)]
         (aset-byte ba i 0))
       (bytes->input t ba))
     true (bytes->input t ba))))

(defn read-input-lines [f samples-per-frame t]
  (if-let [first-line (.readLine f)]
    (let [lines (concat [first-line]
                        (map (fn [_]
                               (if-let [line (.readLine f)]
                                 line
                                 "0,0,0,0,0,0"))
                             (range 1 samples-per-frame)))]
      (csv->input t lines))
    nil))

(defn read-log-trace [path blacklist doms]
  (let [fl (file path)
        fname (.getName fl)
        is-act (.endsWith fname ".act")
        is-csv (.endsWith fname ".csv")
        samples-per-frame 24]
    (when-not (or is-act is-csv)
      (throw (.InvalidArgumentException
              (str "Path " path " is neither ACT nor CSV."))))
    (with-open [f (if is-act (input-stream fl) (reader fl))]
      (let [id (if is-act
                 (.getName (.getParentFile fl))
                 (subs fname 0 (- (count fname) 4)))
            ba (when is-act (byte-array samples-per-frame))]
        (loop [inputs (transient [])
               doms doms]
          (if-let [input (if is-act
                           (read-input-bytes f ba samples-per-frame (count inputs))
                           (read-input-lines f samples-per-frame (count inputs)))]
            (recur (conj! inputs input)
                   (expand-domain input doms))
            [{:id id :inputs (persistent! inputs)} doms]))))))

(defn find-files [path suffix]
  (filter #(.endsWith (.getName %) suffix)
          (file-seq (file path))))

(defn read-logs [path how-many blacklist indoms]
  (let [act-files (find-files path ".act")
        csv-files (find-files path ".csv")
        files (concat act-files csv-files)
        domains (if (nil? indoms) (make-domains) indoms)
        limit (min (count files) (if (= how-many :all)
                                   (count files)
                                   how-many))]
    (loop [ts (transient (hash-map))
           i 0
           doms domains]
      (if (< i limit)
        (if-let [[trace new-doms] (read-log-trace (nth files i)
                                                  blacklist
                                                  doms)]
          (recur
           (assoc! ts (:id trace) trace)
           (inc i)
           new-doms)
          {:traces (persistent! ts) :domains doms})
        {:traces (persistent! ts) :domains doms}))))

(defn sample-data []
  (let [files (filter #(.endsWith (.getName %) "_2.csv")
                      (file-seq (file "resources/traces/ortega_shaker/")))
        [ts ds] (reduce (fn [[traces doms] file]
                          (let [[trace new-doms]
                                (read-log-trace file (hash-set) doms)]
                            [(assoc traces (:id trace) trace) new-doms]))
                        [{} (make-domains)]
                        files)]
    {:traces ts :domains ds}))

(time (sample-data))

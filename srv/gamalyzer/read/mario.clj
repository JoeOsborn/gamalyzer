(ns gamalyzer.read.mario
  (:require [clojure.java.io :refer [input-stream reader file]]
            [clojure.string :refer [split]]
            [clojure.math.numeric-tower :refer [ceil]]
            [gamalyzer.data.input :refer [make-traces make-trace make-input make-domains expand-domain player expand-domain*]])
  (:gen-class :name gamalyzer.read.Mario
              :methods
              [^{:static true} [readLogs ["[Ljava.io.File;"] gamalyzer.data.input.Traces]
               ^{:static true} [readActions [gamalyzer.data.input.Traces bytes] gamalyzer.data.input.Traces]]))

(defn byte->bits [i]
  (reverse (map #(bit-test i %) (range 0 8))))

(defn downsample [[ss lrs js ds] [l r d j s _]]
  [(+ ss (if s 1 0))
   (+ lrs (if l -1 0) (if r 1 0))
   (+ js (if j 1 0))
   (+ ds (if d 1 0))])

(defn samples->input [t quantize samples]
  (make-input t
              :mario
              [:move]
              (seq (map #(int (ceil (/ % quantize)))
                        (reduce downsample
                                [0 0 0 0]
                                samples)))))

(defn bytes->input [t quantize some-bytes]
  (samples->input t quantize
                  (map #(nthrest (byte->bits) 2)
                       some-bytes)))

(defn csv->input [t quantize some-strings]
  (samples->input t quantize
                  (map #(map (fn [s] (not (zero? (read-string s))))
                             (split % #","))
                       some-strings)))

(defn read-input-bytes [f ba samples-per-frame quantize t]
  (let [bytes-read (.read f ba)]
    (cond
     (== bytes-read -1) nil
     (< bytes-read samples-per-frame)
     (do
       (doseq [i (range bytes-read samples-per-frame)]
         (aset-byte ba i 0))
       (bytes->input t quantize ba))
     true (bytes->input t quantize ba))))

(defn read-input-lines [f samples-per-frame quantize t]
  (if-let [first-line (.readLine f)]
    (let [lines (concat [first-line]
                        (map (fn [_]
                               (if-let [line (.readLine f)]
                                 line
                                 "0,0,0,0,0,0"))
                             (range 1 samples-per-frame)))]
      (csv->input t quantize lines))
    nil))

(defn read-log-trace [path blacklist doms]
  (let [fl (file path)
        fname (.getName fl)
        is-act (.endsWith fname ".act")
        is-csv (.endsWith fname ".csv")
        samples-per-frame 24
        quantize 4]
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
                           (read-input-bytes f ba samples-per-frame quantize (count inputs))
                           (read-input-lines f samples-per-frame quantize (count inputs)))]
            (recur (conj! inputs input)
                   (expand-domain input doms))
            (make-traces [(make-trace id (persistent! inputs))]
                         doms)))))))

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
    (loop [ts (transient (vector))
           i 0
           doms domains]
      (if (< i limit)
        (if-let [{[trace] :traces new-doms :domains}
                 (read-log-trace (nth files i)
                                 blacklist
                                 doms)]
          (recur
           (conj! ts trace)
           (inc i)
           new-doms)
          (make-traces (persistent! ts) doms))
        (make-traces (persistent! ts) doms)))))

(defn sample-data
  ([] (sample-data 0))
  ([lev]
   (let [files (filter #(.endsWith (.getName %)
                                   (str "_" lev ".csv"))
                       (file-seq (file "resources/traces/ortega_shaker/")))
         traces-and-domains
         (reduce (fn [{traces :traces
                       doms :domains} file]
                   (let [{[trace] :traces
                          new-doms :domains}
                         (read-log-trace file (hash-set) doms)]
                     (make-traces (conj traces trace) new-doms)))
                 (make-traces [] (make-domains))
                 files)]
     traces-and-domains)))

#_(time (sample-data))


(defn -readLogs [files]
  (reduce (fn [{traces :traces
                doms :domains} file]
            (let [{[trace] :traces
                   new-doms :domains}
                  (read-log-trace file (hash-set) doms)]
              (make-traces (conj traces trace) new-doms)))
          (make-traces [] (make-domains))
          files))

(defn -readActions [{doms :domains} actions]
  ;group actions into chunks of size samples-per-frame
  ;pad the last chunk with 0s
  (let [samples-per-frame 24
        byte-groups (partition samples-per-frame actions)
        last-group (last byte-groups)
        padded-byte-groups (reduce (fn [g _] (conj g 0))
                                   last-group
                                   (range (count last-group)
                                          samples-per-frame))
        inputs (map-indexed bytes->input padded-byte-groups)
        new-doms expand-domain* inputs doms]
    (make-traces [(make-trace :synthetic inputs)] new-doms)))

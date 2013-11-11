(ns gamalyzer.cmp.t-t
  (:require [gamalyzer.cmp.t-t.cdm :as dist]))

(defn diss [s1 s2 doms]
  (dist/diss s1 s2 doms))

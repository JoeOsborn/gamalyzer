(defproject gamalyzer-metric "0.2.0-SNAPSHOT"
  :description "Generic metric distance between game play traces."
  :url "http://github.com/JoeOsborn/gamalyzer"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.apache.commons/commons-compress "1.6"]
                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [net.mikera/vectorz-clj "0.17.0"]
                 [net.mikera/core.matrix "0.15.0"]
                 [clojure-csv/clojure-csv "2.0.1"]]
;  :jar-exclusions [#"(?:^|/).svn/"]
  :aot [gamalyzer.data.input gamalyzer.data.util gamalyzer.read.mario gamalyzer.cmp.tt]
  :repositories {"project" "file:../maven_repository"}
  :plugins []
  :source-paths ["src"])
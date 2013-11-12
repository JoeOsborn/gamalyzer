(defproject gamalyzer "0.1.0-SNAPSHOT"
  :description "Generic visualization of game play traces."
  :url "http://github.com/JoeOsborn/gamalyzer"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/math.combinatorics "0.0.6"]
                 [org.apache.commons/commons-compress "1.6"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]
                 [ring "1.2.1"]
                 [liberator "0.9.0"]
                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :plugins [[lein-ring "0.8.7"]]
  :ring {:handler gamalyzer.handler/app
         :init gamalyzer.handler/init
         :destroy gamalyzer.handler/destroy}
  :profiles
  {:production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}}
   :dev
   {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.2.0"]]}})

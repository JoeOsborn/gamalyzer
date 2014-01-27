(defproject gamalyzer-vis "0.2.0-SNAPSHOT"
  :description "Generic visualization of game play traces."
  :url "http://github.com/JoeOsborn/gamalyzer"
	:license {:name "MIT License"
						:url "http://mit-license.org"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]
                 [ring "1.2.1"]
                 [liberator "0.10.0"]
                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [net.mikera/vectorz-clj "0.17.0"]
                 [net.mikera/core.matrix "0.15.0"]
                 [net.mikera/core.matrix.stats "0.3.0"]
                 [de.uni-konstanz.inf.algo/mdsj "0.2"]
                 [apporiented.com/hierarchical-clustering "1.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [net.drib/strokes "0.5.1"]
								 [gamalyzer-metric "0.2.0-SNAPSHOT"]]
;  :jar-exclusions [#"(?:^|/).svn/"
;	                 #"traces/"]
;  :aot []
  :repositories {"project" {:url "file:../maven_repository"
														:username ""
														:passphrase ""}}
  :plugins [[lein-ring "0.8.7"]
            [lein-cljsbuild "1.0.1"]]
  :source-paths ["srv"]
  :ring {:handler gamalyzer.handler/app
         :init gamalyzer.handler/init
         :destroy gamalyzer.handler/destroy}
  :profiles
  {:production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}}
   :dev
   {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.2.0"]]}}
  :cljsbuild {
    :builds [{:id "cli"
              :source-paths ["cli"]
              :compiler {
                :output-to "resources/public/js/generated/gamalyzer.js"
                :output-dir "resources/public/js/generated"
                :optimizations :none
                :source-map true}}]})

#Gamalyzer#

General-purpose game trace visualization and descriptive analysis. Reads in trace files and spits out pretty pictures via a web server.

##Running the visualization##

Start by running `lein deploy project` from the `metric` directory, then `cd` back to `vis`, `lein cljsbuild once && lein ring server`.

To change the game being visualized, you have to modify `vis/cli/gamalyzer/ui/core.cljs` and change the `mode` variable to something relevant (also provide a default `level` and a range for the `level` slider in the same module); then modify `vis/srv/gamalyzer/data.clj` to support that mode (functions `game-data`, `data`, `data-routes`). Because I don't have permission to distribute the data sets I've used in the past, you're on your own when it comes to data. Sorry!

You might also need to implement a new reader module if you can't output the native Gamalyzer format provided by `gamalyzer.read.edn`. This is a series of EDN terms with a header like `[:properties ([:game game-name] [:events (:i)] ...)]`, where `(:i)` indicates "only inputs are found in this trace" (as opposed to other types of game events). Each individual trace begins with `[:log_start UUID]`, ends with `:log_end`, and consists of a series of terms `[:i player-id decision-module (decision-location-term+) choice-id (choice-term+)]`. Every one of those can be any EDN term, as far as I know. See the paper for details.

##Calculating the metric (Clojure)##

After you've read in some traces and have both a `gamalyzer.data.input.Traces` record and a `gamalyzer.data.input.Domains` record (which is a kind of calculated schema that the traces follow), you can run `gamalyzer.cmp.tt` (trace-to-trace dissimilarity) and call the `(diss-t trace-1 trace-2 domains)` function for a time-varying sequence of pairwise dissimilarities. You may also want to use the `(with-warp-window W ...)` macro from `gamalyzer.cmp.tt.cced` if you plan to use constrained continuous edit distance (CCED); this quantity should be adjusted according to the shape of your data.

All the stuff about pivot selection currently lives in the visualization's `gamalyzer.rsrc.data.process-data` function and its dependencies, but may be lifted into the metric library in time. That module is currently a bit of a grab-bag due to the material requirements of paper-writing.

##Calculating the metric (Java)##

Similar to the above, but you're on your own when it comes to generating a `Traces` and a `Domains` instance (look at how `gamalyzer.read.mario` exposes its functionality for reference). Once you have them, `gamalyzer.cmp.tt` exposes both `diss` (overall dissimilarity) and `dissimilarities` static methods which take as arguments two `Trace` objects, a `Domains`, and a warp window, assuming that CCED is being used.

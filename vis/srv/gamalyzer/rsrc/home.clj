(ns gamalyzer.rsrc.home
  (:require [compojure.core :refer :all]
						[clojure.string :refer [split]]
						[liberator.core :refer [resource defresource]]
						[gamalyzer.data :refer [data]]
            [hiccup.core :refer [html]]))

(defresource home []
  :available-media-types ["text/html" "application/edn"]
  :handle-ok
  (fn [ctx]
		(condp = (get-in ctx [:representation :media-type])
			"text/html"
			(html [:html
						 [:head
							[:link {:rel "stylesheet" :href "/resources/css/d3.slider.css"}]
							[:link {:rel "stylesheet" :href "/resources/css/screen.css"}]
							; [:script {:type "text/javascript" :id "lt_ws" :src "http://localhost:53538/socket.io/lighttable/ws.js"}]
							[:script {:src "/resources/js/generated/goog/base.js" :type "text/javascript"}]
							[:script {:src "/resources/js/d3.js" :type "text/javascript"}]
							[:script {:src "/resources/js/d3.slider.js" :type "text/javascript"}]
							[:script {:src "/resources/js/generated/gamalyzer.js" :type "text/javascript"}]]
						 [:body [:script {:type "text/javascript"} "goog.require(\"gamalyzer.ui.core\")"]]])
			"application/edn"
			(data (rest (split (get-in ctx [:request :uri]) #"/"))
						(get-in ctx [:request :query-params])))))

(defroutes home-routes
  (ANY "/:g" [g] (if (= (str g) "resources") nil (home)))
  (ANY "/:g/*" [g] (if (= (str g) "resources") nil (home))))

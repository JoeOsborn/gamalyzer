(ns gamalyzer.rsrc.home
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]
            [hiccup.core :refer [html]]))

(defresource home []
  :available-media-types ["text/html"]
  :handle-ok
  (fn [ctx] (html [:html
                   [:head
                    [:link {:rel "stylesheet" :href "css/d3.slider.css"}]
                    [:link {:rel "stylesheet" :href "css/screen.css"}]
                    [:script {:type "text/javascript" :id "lt_ws" :src "http://localhost:49917/socket.io/lighttable/ws.js"}]
                    [:script {:src "js/goog/base.js" :type "text/javascript"}]
                    [:script {:src "js/d3.js" :type "text/javascript"}]
                    [:script {:src "js/d3.slider.js" :type "text/javascript"}]
                    [:script {:src "js/gamalyzer.js" :type "text/javascript"}]]
                   [:body [:script {:type "text/javascript"} "goog.require(\"gamalyzer.ui.core\")"]]])))
(defroutes home-routes
  (ANY "/" [] (home)))

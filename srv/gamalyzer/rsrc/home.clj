(ns gamalyzer.rsrc.home
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]))

(defresource home []
  :handle-ok
    (fn [ctx] [:html [:body
                      [:script {:src "resources/public/js/goog/base.js" :type "text/javascript"}]
                      [:script {:src "resources/public/js/cli.js" :type "text/javascript"}]
                      [:script {:type "text/javascript"} "goog.require(\"cli.core\")"]]]))
(defroutes home-routes
  (ANY "/" [] (home)))

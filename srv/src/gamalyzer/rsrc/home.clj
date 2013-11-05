(ns gamalyzer.rsrc.home
  (:require [compojure.core :refer :all]
						[liberator.core :refer [resource defresource]]))

(defresource home []
	:available-media-types ["application/json"]
	:handle-ok
		(fn [ctx]
			["seq" {:k "value" :k2 "val2"}]))

(defroutes home-routes
  (ANY "/" [] (home)))

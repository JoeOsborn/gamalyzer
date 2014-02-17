(ns gamalyzer.handler
  (:require [compojure.core :refer [defroutes routes]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.file-info :refer [wrap-file-info]]
            [hiccup.middleware :refer [wrap-base-url]]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [gamalyzer.rsrc.home :refer [home-routes]]))

(defn init []
  (println "gamalyzer is starting"))

(defn destroy []
  (println "gamalyzer is shutting down"))

(defroutes app-routes
  (route/resources "/resources")
  (route/not-found "Not Found"))

(def app
  (-> (routes home-routes app-routes)
      (handler/site)
      (wrap-base-url)))

(ns gamalyzer.rsrc.home
  (:require [compojure.core :refer :all]
						[clojure.edn :as edn]
						[clojure.java.io :as io]
						[clojure.string :refer [split join]]
						[liberator.core :refer [resource defresource]]
						[gamalyzer.data :refer [data]]
            [hiccup.core :refer [html]]))

;; For PUT and POST check if the content type is (whatever).
(defn check-content-type [ctx content-types]
  (if (#{:put :post} (get-in ctx [:request :request-method]))
    (or
     (some #{(get-in ctx [:request :headers "content-type"])}
           content-types)
     [false {:message "Unsupported Content-Type"}])
    true))

;; convert the body to a reader. Useful for testing in the repl
;; where setting the body to a string is much simpler.
(defn body-as-string [ctx]
  (if-let [body (get-in ctx [:request :body])]
    (condp instance? body
      java.lang.String body
      (slurp (io/reader body)))))

;; For PUT and POST parse the body as edn and store in the context
;; under the given key.
(defn parse-edn [context key]
  (when (#{:put :post} (get-in context [:request :request-method]))
    (try
      (if-let [body (body-as-string context)]
        (let [data (edn/read-string body)]
          [false {key data}])
        {:message "No body"})
      (catch Exception e
        (.printStackTrace e)
        {:message (format "IOException: " (.getMessage e))}))))

#_(edn/read-string "{\"t1\" [ [:i :player :game (:move) :place (2 [6 8])] [:i :player :game (:move) :place (3 [7 9])] ] \"t2\" [ [:i :player :game (:move) :place (2 [6 8])] ] }")

(defn bonus-scripts [ctx]
	(map (fn [nom] [:script {:src (str "/resources/js/" nom ".js")
													 :type "text/javascript"}])
			 ; The params might be an individual item or a list,
			 ; based on how many ?vis=... params there are.
			 (flatten [(get-in ctx [:request :query-params "vis"])])))

(defresource home []
	:allowed-methods [:get :post]
  :available-media-types ["text/html" "application/edn"]
  :handle-ok
  (fn [ctx]
		(condp = (get-in ctx [:representation :media-type])
			"text/html"
			(html [:html
						 (vec (concat [:head
											[:link {:rel "stylesheet" :href "/resources/css/d3.slider.css"}]
											[:link {:rel "stylesheet" :href "/resources/css/screen.css"}]
											; [:script {:type "text/javascript" :id "lt_ws" :src "http://localhost:53538/socket.io/lighttable/ws.js"}]
											[:script {:src "/resources/js/generated/goog/base.js" :type "text/javascript"}]
											[:script {:src "/resources/js/d3.js" :type "text/javascript"}]
											[:script {:src "/resources/js/d3.slider.js" :type "text/javascript"}]
											[:script {:src "/resources/js/generated/gamalyzer.js" :type "text/javascript"}]]
										 (bonus-scripts ctx)))
						 [:body [:script {:type "text/javascript"} "goog.require(\"gamalyzer.ui.core\")"]]])
			"application/edn"
			(data (rest (split (get-in ctx [:request :uri]) #"/"))
						(get-in ctx [:request :query-params]))))
	:known-content-type? #(check-content-type % ["application/edn"])
	:malformed? #(parse-edn % ::data)
	:post!
	(fn [ctx]
		(let [data (::data ctx)
					path (rest (split (get-in ctx [:request :uri]) #"/"))
					dir (io/file (str "resources/traces/" (join "/" (butlast path))))
					filename (io/file dir (str (last path) ".traces"))]
			(.mkdirs dir)
			(println "post" data "to" path)
			; avoid multiple writers overlapping
			(dosync
			 ; maybe it would be better if I could lock just the one file
			 ; instead of "all writers", but...
			 (with-open [w (io/writer filename :append true)]
				 (doseq [trace data
								 t trace]
					 (.write w (pr-str t))
					 (.write w "\n")))))))

(defroutes home-routes
  (ANY "/:g" [g] (if (= (str g) "resources") nil (home)))
  (ANY "/:g/*" [g] (if (= (str g) "resources") nil (home))))

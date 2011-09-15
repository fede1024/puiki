(ns connect.core
 (:use connect.pages
       connect.search
       connect.db
       noir.core
       somnium.congomongo
       clojure.contrib.command-line)
 (:require
       connect.errors
       [noir.server :as server]
       [noir.validation :as vali]
       [noir.session :as session]
       [noir.response :as resp]
   [noir.util.test :as test])
 (:gen-class))

(test/with-noir
   (session/put! :_id "chris")
  (session/get :_id))

(defn valid? [{:keys [firstname lastname]}]
  (vali/rule (vali/min-length? firstname 5)
             [:firstname "Your first name must have more than 5 letters."])
  (vali/rule (vali/has-value? lastname)
             [:lastname "You must have a last name"])
  (not (vali/errors? :lastname :firstname)))

(defpartial channel [[data]]
  [:p data])

;(defpage "/test" {:as user}
;  (valid? {:firstname "pipp" :lastname ""})
;  (html [:div (vali/on-error :firstname channel)
;        (vali/on-error :lastname channel)]))

(def servers (atom {}))

;(defn -main [& m]
;  (let [mode (or (first m) :dev)
;        port (Integer. (get (System/getenv) "PORT" "8080"))]
;    (server/add-middleware connect.errors/wrap-error-check)
;    (mongo! :db "connect")
;    (swap! servers assoc port
;      (server/start port {:mode (keyword mode) :ns 'connect}))))

(defn -main [& args]
  (with-command-line args
    "PoliConnect software."
    [[port "Specify the http port" "8080"]
     remaining]
    (let [p (Integer/parseInt port)]
      (server/add-middleware connect.errors/wrap-error-check)
      (mongo! :db "connect")
      (swap! servers assoc p
        (server/start p {:mode :dev :ns 'connect})))))

(defn stop-server [port]
  (when (@servers port)
    (server/stop (@servers port))
    (swap! servers dissoc port)))

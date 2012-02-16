(ns connect.core
 (:use connect.pages
       connect.search
       connect.db
       noir.core
       somnium.congomongo
       clojure.contrib.command-line)
 (:require
       connect.errors
       connect.logs
       [clojure.contrib.string :as str]
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

;(mongo! :host "dbh42.mongolab.com" :port 27427 :db "connect-test")

(defn in-localhost []
  (= (.getHostName (java.net.InetAddress/getLocalHost))
     "federico-linux"))

(defn -main [& args]
  (with-command-line args
    "Puiki software."
    [[port "Specify the http port" "8080"]
     [mongo-port "Specify the mongodb post" "27017"]
     [mongo-host "Specify the mongodb host" "localhost"]
     [mongo-db   "Specify the mongodb database" "connect"]
     [mongo-user "Specify the mongodb user" "fede"]
     [mongo-pwd  "Specify the mongodb password" "ciao"]
     remaining]
    (let [p (Integer/parseInt port)]
      (server/add-middleware connect.errors/wrap-error-check)
      (server/add-middleware connect.logs/wrap-logging)
      (noir.statuses/set-page! 404
        (connect.pages.utils/js-redirect-page "/not-found"))
      ;(mongo! :db "connect")
      (mongo! :host mongo-host :port (Integer/parseInt mongo-port) :db mongo-db)
      (when (not (str/blank? mongo-user))
        (authenticate mongo-user mongo-pwd))
      (swap! servers assoc p
        (server/start p
          {:ns 'connect
           :session-store (mongo-session :sessions)
           :session-cookie-attrs
           (merge {:expires "Wed, 09 Jun 2021 10:18:14 GMT"}
                  (when (not (in-localhost))
                    {:domain ".puiki.it"}))})))))

(defn stop-server [port]
  (when (@servers port)
    (server/stop (@servers port))
    (swap! servers dissoc port)))

;(map :name (fetch :fields))

;(def indirizzi '("Ingegneria Aerospaziale" "Ingegneria Biomedica" "Ingegneria Chimica e Alimentare"
;                 "Ingegneria Civile" "Ingegneria dei Materiali" "Ingegneria dell'Autoveicolo"
;                 "Ingegneria della Produzione Industriale" "Ingegneria Edile" "Ingegneria Elettrica"
;                 "Ingegneria Energetica" "Ingegneria Meccanica" "Ingegneria per l'Ambiente e il Territorio"
;                 "Matematica per l'Ingegneria" "Ingegneria del Cinema"))

;(fetch :courses :where {:field "Ingegneria Informatica" :year 1})

;(destroy! :courses {:field "Ingegneria Aerospaziale"})

;(dorun 
;  (for [{n :name code :code} (fetch :courses :where {:field "Ingegneria Informatica" :year 1})]
;    (dorun
;      (for [field indirizzi]
;        (do
;          (println "Creo" field n code)
;          (insert! :courses
;            {:name n
;             :field field
;             :year 1
;             :code code
;             :created-by "s162270"
;             :created-at (java.util.Date.)
;             :auto true}))))))

;(dorun 
;  (for [{n :name code :code} (fetch :courses :where {:field "Ingegneria Informatica" :year 1})]
;    (dorun
;      (for [field indirizzi]
;        (println "Creo" field ":" n code)))))

;(for [{name :name code :code} (fetch :courses :where {:field "Ingegneria Informatica" :year 1})]
;  [name code])

;(stop-server 8080)
;(-main)

;(-main "--mongo-db" "connect3" "--mongo-user" "fede" "--mongo-pwd" "47a59b7")
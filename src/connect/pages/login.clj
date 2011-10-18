(ns connect.pages.login
  (:use connect.pages.layout
        connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]
           [noir.util.crypt :as crypt]))

;(for [{id :_id pwd :pwd} (fetch :people)]
;  (update! :people {:_id id}
;    {:$set {:pwd (crypt/encrypt pwd)}}))

(defpage "/login" {:keys [redirect username]}
  (if (current-id)
    (resp/redirect "/")
    (layout "login"
      (form-to [:post "/login"]
        (when redirect
          [:input {:type :hidden :name :redirect :value redirect}])
        [:h2.section "Effettua il login:"]
        (vali/on-error :username error-text)
        [:table
         [:tr [:td.head "Matricola: "]
          [:td (text-field {:placeholder "Username"} :username username)] [:td]]
         [:tr [:td.head "Password: "]
          [:td (password-field {:placeholder "Password"} :password)] [:td]]
         [:tr [:td {:colspan 2}] [:td (submit-button "Login")]]]))))

(defn login! [{:keys [username password] :as user}]
  (let [stored-pass (:pwd (fetch-one :people :where {:_id username}))]
    (if (and stored-pass (crypt/compare password stored-pass))
      (session/put! :username username)
      (vali/set-error :username "Password o nome utente non valido."))))

(defpage [:post "/login"] {:keys [redirect] :as data}
  (if (login! data)
    (resp/redirect (or redirect "/"))
    (render "/login" data)))

(defpage "/logout" {}
  (session/clear!)
  (resp/redirect "/"))

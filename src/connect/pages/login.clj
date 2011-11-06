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

(defpage "/login" {:keys [redirect username]}
  (if (current-id)
    (resp/redirect "/")
    (layout "login"
      (form-to [:post "/login"]
        (when redirect
          [:input {:type :hidden :name :redirect :value redirect}])
        [:h1.section "Login"]
        (vali/on-error :username error-text)
        [:table
         [:tr [:td.head "Matricola: "]
          [:td (text-field {:placeholder "Username"} :username username)]]
         [:tr [:td.head "Password: "]
          [:td (password-field {:placeholder "Password"} :password)]
          [:td (submit-button "Login")]]]
        [:h1.section "Registrazione"]
        [:p "Se non sei ancora registrato clicca " (link-to "/register" "qui") "!"]))))

(defn login! [{:keys [username password]} & [already-encrypted]]
  (let [stored-pass (:pwd (fetch-one :people :where {:_id username}))]
    (if (and stored-pass (if already-encrypted
                           (= password stored-pass)
                           (crypt/compare password stored-pass)))
      (do
        (session/put! :username username)
        (when (session/get :fb-id) ;; Memorizza l'id di facebook
          (update! :people {:_id username}
            {:$set {:fb-id (session/get :fb-id)}})))
      (vali/set-error :username "Password o nome utente non valido."))))

(defpage [:post "/login"] {:keys [redirect] :as data}
  (if (login! data)
    (resp/redirect (or redirect "/"))
    (render "/login" data)))

(defpage "/logout" {}
  (session/clear!)
  (resp/redirect "/"))

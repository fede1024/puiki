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
           [noir.response :as resp]))

(defpartial user-fields [{:keys [username] :as data}]
  (vali/on-error :username error-text)
  (text-field {:placeholder "Username"} :username username)
  (password-field {:placeholder "Password"} :password))

(defpage "/login" {:keys [redirect] :as data}
  (if (current-id)
    (resp/redirect "/")
    (layout "login"
      (form-to [:post "/login"]
        (when redirect
          [:input {:type :hidden :name :redirect :value redirect}])
        [:ul.actions
         [:li (link-to "/" "Home")]]
        (user-fields data)
        (submit-button "Login")))))

(defn login! [{:keys [username password] :as user}]
  (let [stored-pass (:pwd (fetch-one :people :where {:_id username}))]
    (if (and stored-pass (= password stored-pass))
      (do
        ;;(session/put! :admin true)
        (session/put! :username username))
      (vali/set-error :username "Invalid username or password"))))

(defpage [:post "/login"] {:keys [redirect] :as data}
  (if (login! data)
    (resp/redirect (or redirect "/"))
    (render "/login" data)))

(defpage "/logout" {}
  (session/clear!)
  (resp/redirect "/"))

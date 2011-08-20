(ns connect.pages.registration
  (:use connect.pages.layout
        connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(defpartial registration-form [& [data]]
  [:div.registrationForm
   [:h2.register "Inserisci i dati:"]
   (form-to {:accept-charset "utf-8" } [:post "/register"]
     [:table.registerForm
      [:tr [:td.head "Matricola:"]
       [:td (text-field {:size 15} :id (or (:id data) ""))]
       (error-cell :id) [:td]]
      [:tr [:td.head "Password:"]
       [:td (password-field {:size 15} :pwd)]
       (error-cell :pwd) [:td]]
      [:tr [:td.head "Email:"]
       [:td {:colspan 2} 
        (text-field {:size 30} :email (or (:email data) ""))]
       (error-cell :email)]
      [:tr [:td.head "Nome:"]
       [:td {:colspan 2} 
        (text-field {:size 30} :firstname (or (:firstname data) ""))]
       (error-cell :firstname)]
      [:tr [:td.head "Cognome:"]
       [:td {:colspan 2} 
        (text-field {:size 30} :lastname (or (:lastname data) ""))]
       (error-cell :lastname)]
      [:tr [:td.head "Ruolo:"]
       [:td [:input (merge {:type :radio :name "job" :value "student"}
                      (if (= (:job data) "student") {:checked "true"} {})) 
             "Studente"]
        [:input (merge {:type :radio :name "job" :value "professor"}
                  (if (= (:job data) "professor") {:checked "true"} {}))
         "Docente"]]
       (error-cell :job)[:td]]
      [:tr [:td] [:td] [:td (submit-button "Registrati")]]])])

(defpage "/register" {:as user}
  (layout "Registrazione"
    (registration-form user)))

(defn valid? [data]
  (vali/rule (vali/min-length? (:id data) 3)
    [:id "Deve avere più di 3 lettere."])
  (vali/rule (vali/min-length? (:pwd data) 4)
    [:pwd "Deve avere almeno 4 lettere."])
  (vali/rule (not (str/blank? (:email data)))
    [:email "Campo richiesto"])
  (vali/rule (not (str/blank? (:firstname data)))
    [:firstname "Campo richiesto"])
  (vali/rule (not (str/blank? (:lastname data)))
    [:lastname "Campo richiesto"])
  (not (vali/errors? :id :pwd :email :firstname :lastname)))

(defpage [:post "/register"] {:as data}
  (println (pr-str data))
  (if (valid? data)
    (resp/redirect "/")
    (render "/register" data)))

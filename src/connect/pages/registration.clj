(ns connect.pages.registration
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        connect.email
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp])
 (:import [java.util UUID]))

(defpartial registration-form [& [data]]
  (if (current-id)
    [:p "Sei già registrato, username: " (current-id)]
    [:div.registrationForm
     [:h2.section "Inserisci i dati:"]
     (form-to {:accept-charset "utf-8" } [:post "/register"]
       [:table.registerForm
        [:tr [:td.head "Matricola:"]
         [:td (text-field {:size 15} :id (or (:id data) ""))]
         (error-cell :id) [:td "Esempio: s12345"]]
        [:tr [:td.head "Password:"]
         [:td (password-field {:size 15} :pwd)]
         (error-cell :pwd) [:td]]
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
                        ;(if (= (:job data) "student") {:checked "true"} {})
                        {:checked "true"}) 
               "Studente"]
          [:input (merge {:type :radio :name "job" :value "professor" :disabled "true"}
                    (if (= (:job data) "professor") {:checked "true"} {}))
           "Docente"]]
         (error-cell :job) [:td (submit-button "Registrati")]]])]))

(defpage "/register" {:as user}
  (layout "Registrazione"
    (registration-form user)
    [:p "Per verificare la matricola verrà inviata una email a &lt;matricola&gt;@studenti.polito.it"]))

(defn valid? [data]
  (vali/rule (re-matches #"s[0-9]+" (:id data))
    [:id "Matricola non valida"])
  (vali/rule (not (fetch-one :people :where {:_id (:id data)}))
    [:id "Matricola già registrata"])
  (vali/rule (vali/min-length? (:pwd data) 4)
    [:pwd "Deve avere almeno 4 lettere."])
  (vali/rule (not (str/blank? (:firstname data)))
    [:firstname "Campo richiesto"])
  (vali/rule (not (str/blank? (:lastname data)))
    [:lastname "Campo richiesto"])
  (not (vali/errors? :id :pwd :firstname :lastname)))

(defn get-email-address [reg-data]
  (str (:id reg-data) "@studenti.polito.it"))

(defn send-registration-mail [address code]
  (send-mail address "Registrazione PoliConnect"
    (html [:h1 "Benvenuto"]
      [:p "Completa la tua iscrizione cliccando su "
       (link-to (str "http://fede.dyndns-ip.com/register/" code) "questo link") "."])))

(defn new-pending-user [reg-data]
  (let [address (get-email-address reg-data)
        code (str (UUID/randomUUID))]
    (insert! :people-pending
      (merge (dissoc reg-data :id)
        {:_id (:id reg-data) :roles [:user]
         :code code :created-at (java.util.Date.)}))
    (future
      (try
        (send-registration-mail address code)
        (catch Exception e
          (println "Email error." (str e)))))
    address))

(defpage [:post "/register"] {:as reg-data}
  (if (valid? reg-data)
    (let [email (new-pending-user reg-data)]
      (layout "Registrazione"
        [:p "Un'email è stata inviata a " email]
        [:p "Controlla la tua casella di posta elettronica."]))
    (render "/register" reg-data)))

(defpage "/register/:code" {:keys [code]}
  (let [data (fetch-one :people-pending :where {:code code})]
    (if data
      (do
        (insert! :people (dissoc data :code))
        (destroy! :people-pending {:code code})
        (follow-channel (:_id (fetch-one :channels :where {:name "Poli Connect"}))
          (:_id data))
        (connect.pages.login/login! {:username (:_id data) :password (:pwd data)})
        (session/flash-put! :new-user) 
        (resp/redirect (user-edit-path (:_id data))))
      (render "/not-found"))))

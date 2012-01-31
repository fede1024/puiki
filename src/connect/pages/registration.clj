(ns connect.pages.registration
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        connect.email
        connect.logs
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]
           [noir.util.crypt :as crypt]
           [clj-http.client :as client])
 (:import [java.util UUID]))

;(insert! :admin
;  {:_id "recaptcha"
;   :public-key "6LfZQMkSAAAAAK08y4NUQ9X63MvgWG2Vi77iGUoe"
;   :private-key "6LfZQMkSAAAAAEfHR2lQDXAaCc-Q06nPv5NZdHR9"})

(defpartial recaptcha-config []
  [:script {:type "text/javascript"}
   "var RecaptchaOptions = {theme : 'clean', lang : 'it'};"])

(defn recaptcha []
  (let [public-key (:public-key (fetch-one :admin :where {:_id "recaptcha"}))]
    (html
      [:script {:type "text/javascript"
                :src (str "http://www.google.com/recaptcha/api/challenge?k=" public-key)}]
      [:noscript
       [:iframe {:src (str "http://www.google.com/recaptcha/api/noscript?k=" public-key)
                 :height "300" :width "500" :frameborder "0"}] [:br]
       [:input {:type "hidden" :name "recaptcha_response_field"
                :value "manual_challenge"}]])))

(def uri "http://www.google.com/recaptcha/api/verify")

(defn post-request [params]
  (let [{challenge :recaptcha_challenge_field
         response :recaptcha_response_field} params
        query-params {:privatekey (:private-key (fetch-one :admin :where {:_id "recaptcha"}))
                      :challenge challenge :response response
                      :remoteip connect.logs/*ip*}]
    (:body (client/post uri {:query-params query-params}))))

(defn verify-captcha [params]
  (let [result-string (post-request params)
        [status error] (.split result-string "\n" -1)]
    {:status (= status "true") :error-str error}))

(defpartial registration-form [& [data]]
  (recaptcha-config)
  [:div.registrationForm
   (form-to {:accept-charset "utf-8" } [:post "/register"]
     [:table
      (when (admin? (current-id))
        [:tr [:td.head "Email di controllo (solo admin):"]
         [:td (text-field {:size 25} :check-email (or (:check-email data) ""))
          " Per verificare la matricola verrà inviata una email a questo indirizzo."]])
      [:tr [:td.head "Matricola:"]
       [:td (text-field {:size 15} :id (or (:id data) "")) " Esempio: s12345"]
       (error-cell :id)]
      (when-not (admin? (current-id))
        [:tr [:td {:colspan 3} "Per verificare la matricola verrà inviata una email a &lt;matricola&gt;@studenti.polito.it"]])
      [:tr [:td.head "Password:"]
       [:td (password-field {:size 15} :pwd)]
       (error-cell :pwd) [:td]]
      [:tr [:td.head "Conf. password:"]
       [:td (password-field {:size 15} :pwd2)]
       (error-cell :pwd2) [:td]]
      [:tr [:td {:colspan 3} "La password viene memorizzata in modo codificato, quindi ne gli amministratori di "
            "Puiki ne chiunque altro possa avere accesso al database degli utenti è in grado di risalire alla "
            "vostra password."]]
      [:tr [:td.head "Nome:"]
       [:td {:colspan 2} 
        (text-field {:size 30} :firstname (or (:firstname data) ""))]
       (error-cell :firstname)]
      [:tr [:td.head "Cognome:"]
       [:td {:colspan 2} 
        (text-field {:size 30} :lastname (or (:lastname data) ""))]
       (error-cell :lastname)]
      ;[:tr [:td.head "Ruolo:"]
      ; [:td [:input (merge {:type :radio :name "job" :value "student"}
      ;                ;(if (= (:job data) "student") {:checked "true"} {})
      ;                {:checked "true"}) 
      ;       "Studente"]
      ;  [:input (merge {:type :radio :name "job" :value "professor" :disabled "true"}
      ;            (if (= (:job data) "professor") {:checked "true"} {}))
      ;   "Docente"]]
      ; (error-cell :job)]
      [:tr [:td {:colspan 3} (recaptcha)]]
      [:tr (when-let [err (first (vali/get-errors :captcha))]
             [:td.errorMsg [:img.errorMsg {:src "/images/error.png"}] " " err])
       [:td {:colspan 2} (submit-button "Registrati")]]])])

(defpage "/register" {:as user}
  (layout "Registrazione"
    (if (current-id)
      (html
        [:h1.section "Utente già registrato"]
        [:div.sectin [:p "Username: " (link-to (user-info-path (current-id)) (current-id))]]
        (when (admin? (current-id))
          (html
            [:h1.section "Simulazione registrazione (solo admin)"]
            (registration-form user))))
      (html
        [:h1.section "Registrazione"]
        [:p "Registrati a Puiki, è questione di un minuto!"]
        (registration-form user)))))

(defn valid? [data]
  (vali/rule (re-matches #"s[0-9]+" (:id data))
    [:id "Matricola non valida"])
  (vali/rule (and (not (fetch-one :people :where {:_id (:id data)}))
               (not (fetch-one :people-pending :where {:_id (:id data)})))
    [:id "Matricola già registrata"])
  (vali/rule (vali/min-length? (:pwd data) 4)
    [:pwd "Deve avere almeno 4 lettere."])
  (vali/rule (= (:pwd data) (:pwd2 data))
    [:pwd2 "Le password non corrispondono"])
  (vali/rule (not (str/blank? (:firstname data)))
    [:firstname "Campo richiesto"])
  (vali/rule (not (str/blank? (:lastname data)))
    [:lastname "Campo richiesto"])
  (vali/rule (:status (verify-captcha data))
    [:captcha "Stringa errata"])
  (not (vali/errors? :id :pwd :pwd2 :firstname :lastname :captcha)))

(defn get-email-address [reg-data]
  (str (:id reg-data) "@studenti.polito.it"))

(defn send-registration-mail [address code firstname]
  (send-mail address "Registrazione Puiki"
    (html [:h1 "Benvenuto"]
      [:p [:img {:src "http://www.puiki.it/images/logo.png" :style "float: left; padding-right: 10px"}]
       "Grazie per esserti registrato." [:br]
       "Conferma il tuo account cliccando su "
       (link-to (str "http://www.puiki.it/register/" code) "questo link") "." [:br]
       "Se hai ricevuto questa email per errore è sufficiente eliminarla e "
       "non verrai più contattato da " (link-to "http://www.puiki.it" "Puiki") "."])))

(defn new-pending-user [reg-data]
  (let [address (if (admin? (current-id))
                  (:check-email reg-data)
                  (get-email-address reg-data))
        code (str (UUID/randomUUID))]
    (insert! :people-pending
      (merge (dissoc reg-data :id)
        {:_id (:id reg-data) :roles [:user]
         :code code :registered-at (java.util.Date.)}))
    (future
      (try
        (send-registration-mail address code (:firstname reg-data))
        (catch Exception e
          (println "Email error." (str e)))))
    address))

(defpage [:post "/register"] {:as reg-data}
  (if (valid? reg-data)
    (let [email (new-pending-user
                  (merge (dissoc reg-data :pwd2 :recaptcha_challenge_field :recaptcha_response_field)
                    {:pwd (crypt/encrypt (:pwd reg-data)) :job "student"}))]
      (layout "Registrazione"
        [:h1.section "Registrazione avvenuta"]
        [:h2.section "Controlla la tua casella email"]
        [:div.section
         [:p "Un'email è stata inviata a " email]
         [:p "Controlla la tua casella di posta elettronica e clicca sul link per confermare la registrazione."]]))
    (render "/register" reg-data)))

(defpage "/register/:code" {:keys [code]}
  (let [data (fetch-one :people-pending :where {:code code})]
    (if data
      (do
        (insert! :people (merge (dissoc data :code)
                           {:created-at (java.util.Date.)}))
        (destroy! :people-pending {:code code})
        (follow-channel (:_id (fetch-one :channels :where {:name "Puiki"}))
          (:_id data))
        (connect.pages.login/login! {:username (:_id data) :password (:pwd data)}
          :already-encrypted)
        (session/flash-put! "new-user") 
        (resp/redirect (user-edit-path (:_id data))))
      (render "/not-found"))))

(defpartial password-restore-form [& [data]]
  (recaptcha-config)
  [:div.registrationForm
   (form-to {:accept-charset "utf-8" } [:post "/restore-password"]
     [:table
      (when (admin? (current-id))
        [:tr [:td.head "Email di controllo (solo admin):"]
         [:td (text-field {:size 25} :check-email (or (:check-email data) ""))
          " L'email sarà inviata a questo indirizzo."]])
      [:tr [:td.head "Matricola:"]
       [:td (text-field {:size 15} :id (or (:id data) "")) " Esempio: s12345"]
       (error-cell :id)]
      (when-not (admin? (current-id))
        [:tr [:td {:colspan 3} "Per verificare la matricola verrà inviata una email a &lt;matricola&gt;@studenti.polito.it"]])
      [:tr [:td {:colspan 3} (recaptcha)]]
      [:tr (when-let [err (first (vali/get-errors :captcha))]
             [:td.errorMsg [:img.errorMsg {:src "/images/error.png"}] " " err])
       [:td {:colspan 2} (submit-button "Invia email")]]])])

(defpage "/restore-password" {:as user}
  (layout "Registrazione"
    (if (current-id)
      (html
        [:h1.section "Utente già registrato"]
        [:div.sectin [:p "Username: " (link-to (user-info-path (current-id)) (current-id))]]
        (when (admin? (current-id))
          (html
            [:h1.section "Simulazione registrazione (solo admin)"]
            (password-restore-form user))))
      (html
        [:h1.section "Recupero password"]
        [:p "Inserisci il tuo nome utente e ti verrà inviato tramite email un link per cambiare password."]
        (password-restore-form user)))))

(defn restore-valid? [data]
  (vali/rule (re-matches #"s[0-9]+" (:id data))
    [:id "Matricola non valida"])
  (vali/rule (fetch-one :people :where {:_id (:id data)})
    [:id "Matricola non registrata"])
  (vali/rule (:status (verify-captcha data))
    [:captcha "Stringa errata"])
  (not (vali/errors? :id :captcha)))

(defn send-password-restore-mail [address code]
  (send-mail address "Puiki: password dimenticata"
    (html [:h1 "Password dimenticata?"]
      [:p [:img {:src "http://www.puiki.it/images/logo.png" :style "float: left; padding-right: 10px"}]
       "Se hai simenticato la password clicca su "
       (link-to (str "http://www.puiki.it/restore-password/" code) "questo link") "." [:br]
       "Se hai ricevuto questa email per errore è sufficiente eliminarla e "
       "non verrai più contattato da " (link-to "http://www.puiki.it" "Puiki") "."])))

(defn new-restore-procedure [data]
  (let [address (if (admin? (current-id))
                  (:check-email data)
                  (get-email-address data))
        code (str (UUID/randomUUID))]
    (update! :restore-procedure {:_id (:id data)}
        {:_id (:id data) :code code
         :created-at-at (java.util.Date.)})
    (future
      (try
        (send-password-restore-mail address code)
        (catch Exception e
          (println "Email error." (str e)))))
    address))

(defpage [:post "/restore-password"] {:as data}
  (if (restore-valid? data)
    (let [email (new-restore-procedure data)]
      (layout "Recupero password"
        [:h1.section "Operazione avvenuta"]
        [:h2.section "Controlla la tua casella email"]
        [:div.section
         [:p "Un'email è stata inviata a " email]
         [:p "Controlla la tua casella di posta elettronica e clicca sul link per confermare la modifica della password."]]))
    (render "/restore-password" data)))

(defpartial change-pwd-form [code]
  [:div.registrationForm
   (form-to {:accept-charset "utf-8" } [:post (str "/restore-password/" code)]
     [:table
      [:tr [:td.head "Password:"]
       [:td (password-field {:size 15} :pwd)]
       (error-cell :pwd) [:td]]
      [:tr [:td.head "Conf. password:"]
       [:td (password-field {:size 15} :pwd2)]
       (error-cell :pwd2) [:td]]
      [:tr [:td {:colspan 2} (submit-button "Modifica password")]]])])

(defpage "/restore-password/:code" {:keys [code]}
  (let [data (fetch-one :restore-procedure :where {:code code})]
    (if data
      (layout "Modifica password"
        [:h1.section "Modifica dati utente"]
        [:h2.section "Scrivi la tua nuova password (matricola " (:_id data) ")"]
        (change-pwd-form code))
      (render "/not-found"))))

(defn change-pwd-valid? [data]
  (vali/rule (vali/min-length? (:pwd data) 4)
    [:pwd "Deve avere almeno 4 lettere."])
  (vali/rule (= (:pwd data) (:pwd2 data))
    [:pwd2 "Le password non corrispondono"])
  (not (vali/errors? :pwd :pwd2)))

(defpage [:post "/restore-password/:code"] {:keys [code] :as data}
  (if (change-pwd-valid? data)
    (let [{id :_id} (fetch-one :restore-procedure :where {:code code})]
      (update! :people {:_id id}
         {:$set {:pwd (crypt/encrypt (:pwd data))}})
      (destroy! :restore-procedure {:code code})
      (layout "Password modificata"
        [:h1.section "Procedura completata"]
        [:h2.section "Effettua il login"]
        [:p "La password relativa all'utente " id " è stata modificata."]
        [:p "Ora puoi effettuare il " (link-to "/login" "login") "."]))
    (render "/restore-password/:code" data)))

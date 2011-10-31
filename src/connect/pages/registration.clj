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

(def recaptcha-config
  (html [:script {:type "text/javascript"}
         "var RecaptchaOptions = {theme : 'clean', lang : 'it'};"]))

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

(def *uri* "http://www.google.com/recaptcha/api/verify")

(defn post-request [params]
  (let [{challenge :recaptcha_challenge_field
         response :recaptcha_response_field} params
        query-params {:privatekey (:private-key (fetch-one :admin :where {:_id "recaptcha"}))
                      :challenge challenge :response response
                      :remoteip connect.logs/*ip*}]
    (:body (client/post *uri* {:query-params query-params}))))

(defn verify-captcha [params]
  (let [result-string (post-request params)
        [status error] (.split result-string "\n" -1)]
    {:status (= status "true") :error-str error}))

(defpartial registration-form [& [data]]
  (if (current-id)
    [:p "Sei già registrato, username: " (current-id)]
    (html
      [:h1.section "Registrazione"]
      recaptcha-config
      [:div.registrationForm
       (form-to {:accept-charset "utf-8" } [:post "/register"]
         [:table
          [:tr [:td.head "Matricola:"]
           [:td (text-field {:size 15} :id (or (:id data) "")) " Esempio: s12345"]
           (error-cell :id)]
          [:tr [:td {:colspan 3} "Per verificare la matricola verrà inviata una email a &lt;matricola&gt;@studenti.polito.it"]]
          [:tr [:td.head "Password:"]
           [:td (password-field {:size 15} :pwd)]
           (error-cell :pwd) [:td]]
          [:tr [:td.head "Conf. password:"]
           [:td (password-field {:size 15} :pwd2)]
           (error-cell :pwd2) [:td]]
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
           (error-cell :job)]
          [:tr [:td {:colspan 3} (recaptcha)]]
          [:tr (when-let [err (first (vali/get-errors :captcha))]
                 [:td.errorMsg [:img.errorMsg {:src "/images/error.png"}] " " err])
           [:td {:colspan 2} (submit-button "Registrati")]]])])))

(defpage "/register" {:as user}
  (layout "Registrazione"
    (registration-form user)))

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

(defn send-registration-mail [address code]
  (send-mail address "Registrazione PoliConnect"
    (html [:h1 "Benvenuto"]
      [:p "Grazie per esserti registrato."]
      [:p "Conferma il tuo account cliccando su "
       (link-to (str "http://fede.dyndns-ip.com/register/" code) "questo link") "."]
      [:p "Se hai ricevuto questa email per errore è sufficiente eliminarla e "
       "non verrai più contattato da PoliConnect."])))

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
    (let [email (new-pending-user
                  (merge (dissoc reg-data :pwd2 :recaptcha_challenge_field :recaptcha_response_field)
                    {:pwd (crypt/encrypt (:pwd reg-data))}))]
      (layout "Registrazione"
        [:p "Un'email è stata inviata a " email]
        [:p "Controlla la tua casella di posta elettronica."]))
    (render "/register" reg-data)))

(defpage "/register/:code" {:keys [code]}
  (let [data (fetch-one :people-pending :where {:code code})]
    (if data
      (do
        (insert! :people (dissoc data :code)) ;; FIXME: data di registrazione = data conferma email
        (destroy! :people-pending {:code code})
        (follow-channel (:_id (fetch-one :channels :where {:name "Poli Connect"}))
          (:_id data))
        (connect.pages.login/login! {:username (:_id data) :password (:pwd data)}
          :already-encrypted)
        (session/flash-put! :new-user) 
        (resp/redirect (user-edit-path (:_id data))))
      (render "/not-found"))))

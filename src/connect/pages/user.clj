(ns connect.pages.user
  (:use connect.pages.layout
        connect.pages.utils
        connect.pages.channel
        connect.db
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

(pre-route "/user*" p
  (if (current-id)
    (if (not (user? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (:uri p)})))

(defpage "/user/" []
  (layout "Pagina utente"
    [:h2.section "Informazioni personali:"]
    [:p (link-to (user-info-path (current-id)) "Vedi")]
    [:p (link-to (user-edit-path (current-id)) "Modifica")]
    [:h2.section "Canali:"]
    [:p (link-to "/user/following" "Seguiti")]
    [:p (link-to "/channel/list" "Tutti")]
    [:h2.section "Utenti:"]
    [:p (link-to "/user/list" "Elenco utenti")]))

(defpartial student-info [person]
  (let [channel (fetch-one :channels :where {:field (:field person) :year (:year person)})]
    [:table
     [:tr [:td.head "Indirizzo:"]
      [:td (:field person)]]
     [:tr [:td.head "Anno immatr.:"] [:td (:year person)]]
     [:tr [:td.head "Canale associato: "] ;; TODO: Non mettere qui
      [:td (if (and (:field person) channel)
             (link-to (channel-path channel) (:name channel))
             "Nessuno.")]]]))

(defpage "/user/:id/info" {:keys [id]}
  (layout "Informazioni utente"
    (if (= (session/flash-get) :done) 
      [:p "I dati sono stati modificati"])
    [:h2.section (str "Informazioni utente " id)]
    (let [person (fetch-one :people :where {:_id id})]
      (if person
        [:span
         (cond (= (:job person) "student")
           (student-info person)
           (= (:job person) "professon")
           [:p "TODO info docente"]
           (= (:job person) "admin")
           [:p "TODO info admin"])
         [:p (link-to (user-edit-path (current-id)) "Modifica")]]
        "Utente non esistente."))))

(defpage "/user/list" []
  (layout "Elenco utenti"
    [:h2.section "Elenco utenti"]
    (people-table (fetch :people :sort {:lastname 1 :firstname 1})
      :lastname true :info true)))

(defn to-integer [value]
  (if value 
    (if (integer? value)
      value
      (try (Integer/parseInt value)
        (catch Exception e 0)))
    0))

(defpartial student-edit-form [person & [data]] ;; TODO: fix data
  (form-to {:accept-charset "utf-8"} [:post (user-edit-path (:_id person))]
    [:table
     [:tr [:td.head "Indirizzo: "]
      [:td (drop-down :field (map :name (fetch :fields))
             (if data (:field data) (:field person)))]
      (error-cell :field)]
     [:tr [:td.head "Anno immatr.: "]
      [:td (drop-down :year (range 2003 2022)
             (to-integer (if data
                           (:year data)
                           (:year person))))]
      (error-cell :year)]
     [:tr [:td] [:td (submit-button "Salva")]]]))

(defpartial professor-edit-form [id & [data vd]]
  [:p "TODO: da fare"])

(defpage "/user/:id/edit" {:keys [id]}
  (layout "Modifica utente"
    [:h2.register "Modifica i dati di " id ":"]
    (let [person (fetch-one :people :where {:_id id})]
      (if person
        (if (or (admin? (current-id)) (= id (current-id)))
          (if (= (:job person) "student")
            (student-edit-form person)
            (professor-edit-form person))
          "Non hai l'autorizzazione per modificare i dati.")
        "Utente non esistente."))))

(defn get-course-year [year]
  (- 2012 year))

(defn field-channel-name [field-name year]
  (str field-name " " (get-course-year year) "°anno"))

(defn field-channel-description [field-name year]
  (str "Canale di " field-name " " (get-course-year year) "°anno."))

(defn create-field-channel [field-name year]
  (let [channel (fetch-one :channels :where {:field field-name :year year})]
    (or channel
      (insert! :channels
        {:name (field-channel-name field-name year) :privacy :public
         :description (field-channel-description field-name year)
         :type :field   :field field-name
         :year year     :followers 0
         :created-at (java.util.Date.)}))))

;(defn follow-channel [channel-id person-id]
;  (update! :people {:_id person-id}
;    {:$addToSet {:follows channel-id}})
;  (update! :channels {:_id channel-id}
;    {:$inc {:followers 1}}))

(defn valid? [{:keys [id field year]}]
  (vali/rule (vali/has-value? field)
     [:name "Il campo non deve essere vuoto."])
  (vali/rule (fetch-one :fields :where {:name field})
     [:name "Indirizzo non esistente."])
  (vali/rule (fetch-one :people :where {:_id id})
     [:id "Matricola non valida."]) ;; Non visualizzato
  (vali/rule (let [y (Integer/parseInt year)]
               (and (> y 2003) (< y 2011)))
     [:year "Anno di immatricolazione non valido."])
  (not (vali/errors? :field :year :id)))

(defpage [:post "/user/:id/edit"] {:keys [id year field] :as person}
  (if (not (valid? person))
    (render (user-edit-path id))
    (let [y (Integer/parseInt year)]
      (update! :people {:_id id}
        {:$set {:year y :field field}})
      (follow-channel (:_id (create-field-channel field y)) id)
      (session/flash-put! :done)
      (resp/redirect (user-info-path id)))))

(defpage "/user/following" []
  (layout "Canali seguiti"
;    (map channel-table
;      (map #(fetch-one :channels :where {:_id %})
;        (:follows (fetch-one :people :where {:_id (current-id)}))))
    (let [channels (map #(fetch-one :channels :where {:_id %})
                     (:follows (fetch-one :people :where {:_id (current-id)})))]
      (html
        [:h2.section "Indirizzi di studio:"]
        (for [c (filter #(= (:type %) "field") channels)]
          [:p [:img {:src "/images/dot.png" :height 10}] " "
           (link-to (channel-path c) (:name c))
           [:span.channelInfo (channel-info c)]])
        [:h2.section "Gruppi:"]
        (for [c (filter #(= (:type %) "group") channels)]
          [:p [:img {:src "/images/dot.png" :height 10}] " "
           (link-to (channel-path c) (:name c))
           [:span.channelInfo (channel-info c)] [:br]
           [:span.channelDescription (:description c)]])))))

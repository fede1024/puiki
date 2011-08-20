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
    [:h2 "Informazioni personali:"]
    [:p (link-to (user-info-path (current-id)) "Vedi")]
    [:p (link-to (user-edit-path (current-id)) "Modifica")]
    [:h2 "Canali:"]
    [:p (link-to "/user/following" "Seguiti")]
    [:p (link-to "/channel/list" "Tutti")]
    [:h2 "Utenti:"]
    [:p (link-to "/user/list" "Elenco utenti")]))

(defpartial student-info [person]
  (let [c-id (when (:field person)
               (get-in (unref (:field person))
                 [:channels (keyword (:year person))]))]
    [:table
     [:tr [:td.head "Indirizzo:"]
      [:td (:name (unref (:field person)))]]
     [:tr [:td.head "Anno immatr.:"] [:td (:year person)]]
     [:tr [:td.head "Canale associato: "] ;; TODO: Non mettere qui
      [:td (if c-id
             (if-let [channel (unref c-id)]
               (link-to (channel-path channel) (:name channel))
               "Nessuno.")
             "Nessuno.")]]]))

(defpage "/user/:id/info" {:keys [id]}
  (layout "Informazioni utente"
    (if (= (session/flash-get) :done) 
      [:p "I dati sono stati modificati"])
    [:h2 (str "Informazioni utente " id)]
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
    [:h2.register "Elenco utenti"]
    (people-table (fetch :people))))

(defn to-integer [value]
  (if value 
    (if (integer? value)
      value
      (try (Integer/parseInt value)
        (catch Exception e 0)))
    0))

(defpartial student-edit-form [person & [data]] ;;FIX: data?
  (form-to {:accept-charset "utf-8"} [:post (user-edit-path (:_id person))]
    [:table
     [:tr [:td.head "Indirizzo: "]
      [:td (drop-down :field (map :name (fetch :fields))
             (if data (:field data) (:name (unref (:field person)))))]
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
  (- 2012 (Integer/parseInt year)))

(defn field-channel-name [field year]
  (str (:name field) " " (get-course-year year) "° anno"))

(defn field-channel-description [field year]
  (str "Canale di " (:name field) " " (get-course-year year) "° anno."))

(defn create-field-channel [field year]
  (let [channel (unref (get (:channels field) (keyword year)))]
    (or channel
      (let [new (insert! :channels
                  {:name (field-channel-name field year) :privacy :public
                   :description (field-channel-description field year)
                   :type :field
                   :type_ref (db-ref :fields (:_id field))
                   :followers #{}
                   :created-at (java.util.Date.)})]
        (update! :fields {:_id (:_id field)}
          {:$set {:channels (merge (:channels field) ;; TODO: fix?
                              {(keyword year) (db-ref :channels (:_id new))})}})
        new))))

(defn follow-channel [channel-id person-id]
  (update! :people {:_id person-id}
    {:$addToSet {:follows (db-ref :channels channel-id)}})
  (update! :channels {:_id channel-id}
    {:$addToSet {:followers (db-ref :people person-id)}}))

(defn valid? [{:keys [id field year]}]
  (vali/rule (vali/has-value? field)
     [:name "Il campo non deve essere vuoto."])
  (vali/rule (fetch-one :fields :where {:name field})
     [:name "Indirizzo non esistente."])
  (vali/rule (fetch-one :people :where {:_id id})
     [:id "Matricola non valida."])
  (vali/rule (let [y (Integer/parseInt year)]
               (and (> y 2003) (< y 2011)))
     [:year "Anno di immatricolazione non valido."])
  (not (vali/errors? :field :year)))

(defpage [:post "/user/:id/edit"] {:keys [id year field] :as person}
  (if (not (valid? person))
    (resp/redirect (user-edit-path id))
    (let [field (fetch-one :fields :where {:name field})]
      (update! :people {:_id id}
        {:$set {:year year :field (db-ref :fields (:_id field))}})
      (follow-channel (:_id (create-field-channel field year)) id)
      (session/flash-put! :done)
      (resp/redirect (user-info-path id)))))

(defpage "/user/following" []
  (layout "Canali seguiti"
    [:h2 "Stai seguendo i canali:"]
    (map channel-table
      (map unref
        (:follows (fetch-one :people :where {:_id (current-id)}))))))
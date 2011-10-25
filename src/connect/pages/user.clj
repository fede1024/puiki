(ns connect.pages.user
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [connect.pages.channel :as channel]
           [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(pre-route "/user*" request
  (if (current-id)
    (if (not (user? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (get-request-uri request)})))

(defpage "/user/" []
  (layout "Pagina utente"
    [:h1.section "Pagina utente"]
    [:h2.section "Informazioni personali:"]
    [:div.section
     [:p (link-to (user-info-path (current-id)) "Vedi")]
     [:p (link-to (user-edit-path (current-id)) "Modifica")]]
    [:h2.section "Canali:"]
    [:div.section
     [:p (link-to "/user/following" "Notifiche")]
     [:p (link-to "/channel/list" "Modifica i canali seguiti")]]
    [:h2.section "Utenti:"]
    [:div.section
     [:p (link-to "/user/list" "Elenco utenti")]]))

(defpartial student-info [person]
  [:h2.section "Account:"]
  [:table
   [:tr [:td.head "Nome:"]    [:td (:firstname person)]]
   [:tr [:td.head "Cognome:"] [:td (:lastname person)]]
   [:tr [:td.head "Data iscrizione: "]
    [:td (format-timestamp (:created-at person))]]]
  [:h2.section "Indirizzo di studio:"]
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
    [:h1.section "Informazioni utente"]
    (let [person (fetch-one :people :where {:_id id})]
      (if person
        [:span
         (cond (= (:job person) "student")
           (student-info person)
           (= (:job person) "professon")
           [:p "TODO info docente"]
           (= (:job person) "admin")
           [:p "TODO info admin"])
         [:h2.section (link-to (user-edit-path (current-id)) "Modifica")]]
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

(defpartial account-edit-form [person & [data]] ;; TODO: fix data
  [:h2.section "Account:"]
  [:table
   [:tr [:td.head "Nome: "]
    [:td (text-field :firstname (or (:firstname data)
                                  (:firstname person)))]
    (error-cell :firstname)]
   [:tr [:td.head "Cognome: "]
    [:td (text-field :lastname (or (:lastname data)
                                  (:lastname person)))]
    (error-cell :lastname)]])

(defpartial field-edit-form [person & [data]] ;; TODO: fix data
  [:h2.section "Indirizzo di studio:"]
  [:table
   [:tr [:td.head "Indirizzo: "]
    [:td (drop-down :field (map :name (fetch :fields))
           (or (:field data) (:field person)))]
    (error-cell :field)]
   [:tr [:td.head "Anno immatr.: "]
    [:td (drop-down :year (range 2007 2012)
           (to-integer (or (:year data) (:year person))))]
    (error-cell :year)]])

(defpartial student-edit-form [person & [data]] ;; TODO: fix data
  [:h1.section "Modifica dati utente"]
  (form-to {:accept-charset "utf-8"} [:post (user-edit-path (:_id person))]
    (account-edit-form person data)
    (field-edit-form person data)
    (submit-button "Salva dati")))

(defpartial professor-edit-form [id & [data vd]]
  [:p "TODO: da fare"])

(defpage "/user/:id/edit" {:keys [id] :as data}
  (layout "Modifica utente"
    (if (= (session/flash-get) :new-user) 
      [:p "Nuovo utente registrato"])
    (let [person (fetch-one :people :where {:_id id})]
      (if person
        (if (or (admin? (current-id)) (= id (current-id)))
          (if (= (:job person) "student")
            (student-edit-form person data)
            (professor-edit-form person data))
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
         :year year     :created-at (java.util.Date.)}))))

(defn valid? [{:keys [id field year firstname lastname]}]
  (vali/rule (vali/has-value? field)
     [:name "Il campo non deve essere vuoto."])
  (vali/rule (fetch-one :fields :where {:name field})
     [:name "Indirizzo non esistente."])
  (vali/rule (fetch-one :people :where {:_id id})
     [:id "Matricola non valida."]) ;; Non visualizzato
  (vali/rule (vali/has-value? firstname)
     [:firstname "Il campo non deve essere vuoto."])
  (vali/rule (vali/has-value? lastname)
     [:lastname "Il campo non deve essere vuoto."])
  (vali/rule (let [y (Integer/parseInt year)]
               (and (>= y 2007) (<= y 2012)))
     [:year "Anno di immatricolazione non valido."])
  (not (vali/errors? :field :year :firstname :lastname :id)))

(defpage [:post "/user/:id/edit"] {:keys [id year field firstname lastname] :as person}
  (if (not (valid? person))
    (render "/user/:id/edit" person)
    (let [y (Integer/parseInt year)]
      (update! :people {:_id id}
        {:$set {:year y :field field :firstname firstname :lastname lastname}})
      (follow-channel (:_id (create-field-channel field y)) id)
      (session/flash-put! :done)
      (resp/redirect (user-info-path id)))))

(defpage "/user/following" []
  (layout "Canali seguiti"
    (let [user (fetch-one :people :where {:_id (current-id)})
          channels (map #(fetch-one :channels :where {:_id %})
                     (:follows user))
          fields (filter #(= (:type %) "field") channels)
          groups (filter #(= (:type %) "group") channels)
          courses (filter #(= (:type %) "course") channels)
          new-posts (filter #(= (:action %) "new-post") (:news user))
          new-answers (filter #(= (:action %) "new-answer") (:news user))
          new-comments (filter #(= (:action %) "new-comment") (:news user))]
      (html
        ;[:h1.section "Notifiche: " (count (:news user))]
        [:h1.section "Nuovi post: " (count new-posts)]
        [:div.section
         [:p (link-to "/channel/list" "Modifica canali seguiti")]
         (let [groups (group-by :channel new-posts)]
           (for [[channel group] groups]
             (html [:h2.section (:name (fetch-one :channels :where {:_id channel}))]
             (channel/post-links
               (map #(fetch-one :posts :where {:_id (:post %)}) group)))))
         [:p "Nuove risposte ai tuoi post: " (count new-answers)]
         (for [n new-answers]
           [:p [:img {:src "/images/dot.png" :height 10}] " "
            (link-to (str "/post/" (:post n)) (:title n))
            " - "  (:question-title n)])
         [:p "Nuovi commenti ai tuoi post: " (count new-comments)]
         (for [n new-comments]
           [:p [:img {:src "/images/dot.png" :height 10}] " "
            (link-to (str "/post/" (:post n)) (:title n))])]
        [:h1.section "Canali seguiti:"]
        [:div.section
         [:h2.section "Indirizzi di studio: " (count fields)]
         [:ul.channels
          (for [c fields]
            [:li.channel [:img {:src "/images/dot.png" :height 10}] " "
             (link-to (channel-path c) (:name c))
             [:p.channelInfo (channel/channel-info c)]])]
         [:h2.section "Corsi: " (count courses)]
         [:ul.channels
          (for [c courses]
            [:li.channel [:img {:src "/images/dot.png" :height 10}] " "
             (link-to (channel-path c) (:name c))
             [:p.channelInfo (channel/channel-info c)]])]
         [:h2.section "Gruppi: " (count groups)]
         [:ul.channels
          (for [c groups]
            [:li.channel [:img {:src "/images/dot.png" :height 10}] " "
             (link-to (channel-path c) (:name c))
             [:p.channelInfo (channel/channel-info c)]
             [:p.channelDescription (:description c)]])]]))))

(defpage "/user/new-course" {:keys [field name]:as data}
  (layout "Nuovo corso di studi"
    (form-to {:accept-charset "utf-8"} [:post "/user/new-course"]
      [:table
       [:tr [:td.head "Indirizzo: "]
        [:td (drop-down :field (map :name (fetch :fields))
               field)]
        (error-cell :field)]
       [:tr [:td.head "Nome corso: "]
        [:td (text-field {:placeholder "Nome corso"} :name
                (or (:name data) ""))]
        (error-cell :name)]
       [:tr [:td] [:td (submit-button "Aggiungi")]]])))

(defn course-channel-description [field name]
  (str name " (" field ")"))

(defn create-course-channel! [field name]
  (insert! :channels
    {:name name :privacy :public
     :description (course-channel-description field name)
     :type :course :created-at (java.util.Date.)}))

(defn create-course! [field name channel]
  (insert! :courses
    {:name name
     :channel channel
     :field field
     :created-by (current-id)
     :created-at (java.util.Date.)}))

(defn valid-course? [{:keys [field name]}]
  (vali/rule (vali/has-value? field)
    [:field "Il campo non deve essere vuoto."])
  (vali/rule (fetch-one :fields :where {:name field})
    [:field "Indirizzo non esistente."])
  (vali/rule (not (fetch-one :courses :where {:field field :name name}))
    [:name "Corso già esistente."])
  (vali/rule (not (str/blank? name))
    [:name "Nome corso non valido"])
  (not (vali/errors? :field :name)))

(defpage [:post "/user/new-course"] {:keys [field name] :as data}
  (if (current-id)
    (if (valid-course? data)
      (let [channel (create-course-channel! field name)]
        (create-course! field name ;; Crea il nuovo canale comunque
          (:_id channel))
        (session/flash-put! :new)
        (resp/redirect (channel-path channel)))
      (render "/user/new-course" data))
    (render "/permission-denied")))

(defpage "/user/feedback" []
  (layout ""
    [:h1.section "Feedback"]
    [:p "Se vuoi riportare un errore, un malfunzionamento, un suggerimento qualsiasi "
     "scrivi nella casella sottostante, oppure mandami un "
     (link-to "mailto:giraud.federico@gmail.com" "email") "."]
    [:p "Grazie per il tuo aiuto!"]
    (form-to {:accept-charset "utf-8" } [:post "/user/feedback"]
      (text-area {:class :postComment :rows 10 :placeholder "Vorrei che..."} :text)
      (submit-button "Invia!"))))

(defpage [:post "/user/feedback"] {:keys [text]}
  (if (current-id)
    (do
      (insert! :feedbacks
        {:person (current-id) :text text :created-at (java.util.Date.)})
      (layout ""
        [:h1.section "Grazie!"]
        [:p "Il tuo suggerimento è stato memorizzato."]
        [:p (link-to "/" "Home")]))
    (render "/permission-denied")))
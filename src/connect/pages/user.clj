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
     [:p (link-to "/user/following" "Notifiche")]]
    [:h2.section "Utenti:"]
    [:div.section
     [:p (link-to "/user/list" "Elenco utenti")]]))

(defn get-person-study-year [person]
  (when (:year person)
    (min 5 (- 2012 (:year person)))))

(defpartial student-info [person]
  [:h2.section "Account:"]
  [:table
   [:tr [:td.head "Nome:"]    [:td (:firstname person)]]
   [:tr [:td.head "Cognome:"] [:td (:lastname person)]]
   [:tr [:td.head "Data iscrizione: "]
    [:td (format-timestamp (:created-at person))]]]
  [:h2.section "Indirizzo di studio:"]
  (let [channel (fetch-one :channels :where {:field (:field person)
                                             :year (get-person-study-year person)})]
    [:table
     [:tr [:td.head "Indirizzo:"]
      [:td (:field person)]]
     [:tr [:td.head "Anno immatr.:"]
      [:td (when (:year person)
             (str (:year person) " (" (get-person-study-year person) "° anno)"))]]
     [:tr [:td.head "Canale associato: "]
      [:td (if (and (:field person) channel)
             (link-to (channel-path channel) (:name channel))
             "Nessuno.")]]]))

(defpage "/user/:id/info" {:keys [id]}
  (layout "Informazioni utente"
    (if (= (session/flash-get) "done") 
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
         (when (or (admin? (current-id)) (= (current-id) id)) 
           [:h2.section (link-to (user-edit-path id) "Modifica")])]
        "Utente non esistente."))))

(defpage "/user/list" []
  (let [users (sort-by #(.toLowerCase (or (:lastname %) ""))
                 (sort-by #(.toLowerCase (or (:firstname %) ""))
                   (fetch :people)))]
    (layout "Elenco utenti"
      [:h2.section "Utenti registrati: (" (count users)")"]
      (people-table users :lastname true :field true :img true
                    :link (not (nil? (current-id)))))))

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
  (let [person (fetch-one :people :where {:_id id})]
    (layout "Modifica utente"
      (when (= (session/flash-get) "new-user") 
        (html
          [:h1.section "Benvenuto!"]
          [:p "Grazie per esserti registrato. Ora fai parte di PoliConnect."]))
      (if person
        (if (or (admin? (current-id)) (= id (current-id)))
          (if (= (:job person) "student")
            (student-edit-form person data)
            (professor-edit-form person data))
          "Non hai l'autorizzazione per modificare i dati.")
        "Utente non esistente."))))

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
    (let [y (Integer/parseInt year)
          channel (fetch-one :channels :where {:field field :year (- 2012 y)})]
      (update! :people {:_id id}
        {:$set {:year y :field field :firstname firstname :lastname lastname}})
      (when channel (follow-channel (:_id channel) id))
      (session/flash-put! "done")
      (resp/redirect (user-info-path id)))))

(defpage "/user/following" {:keys [remove-news]}
  (when (and (current-id) (= remove-news "true"))
    (update! :people {:_id (current-id)}
      {:$set {:news []}}))
  (let [user (fetch-one :people :where {:_id (current-id)})
        channels (sort-by :name
                   (map #(fetch-one :channels :where {:_id %})
                        (:follows user)))
        fields (filter #(= (:type %) "field") channels)
        groups (filter #(= (:type %) "group") channels)
        courses (filter #(= (:type %) "course") channels)
        news (reverse (sort-by :created-at (:news user)))
        new-posts (filter #(= (:action %) "new-post") news)
        new-answers (filter #(= (:action %) "new-answer") news)
        new-comments (filter #(= (:action %) "new-comment") news)]
    (layout "Canali seguiti"
      [:h1.section "Notifiche:"]
      [:div.section
       (if (empty? news)
         [:p "Nessuna."]
         [:p.right [:img.middle {:src "/images/remove.png"}] " "
          [:a {:href (encode-url "/user/following" {:remove-news true})}
           "Rimuovi tutte le notifiche"]])
       (let [groups (reverse (sort-by first (group-by :channel new-posts)))]
         (for [[channel group] groups]
           (html [:h2.section "Nuovi post di " (:name (fetch-one :channels :where {:_id channel}))]
                 [:div.section
                 (channel/post-links
                   (map #(fetch-one :posts :where {:_id (:post %)}) group) :show-removed)])))
       (when (not (empty? new-answers))
         (html [:h2.section "Nuove risposte ai tuoi post:"]
           (channel/post-links
             (map #(fetch-one :posts :where {:_id (:post %)}) new-answers) :show-removed)))
       (when (not (empty? new-comments))
         (html [:h2.section "Nuovi commenti ai tuoi post:"]
           (channel/post-links
             (map #(fetch-one :posts :where {:_id (:post %)}) new-comments) :show-removed)))]
      [:h1.section "Canali seguiti:"]
      [:div.section
       [:h2.section "Indirizzi di studio: " (count fields)]
       [:ul.channels
        (for [c fields]
          [:li.channel (link-to (channel-path c) (:name c))
           ;[:p.channelInfo (channel/channel-info c)]
           ])]
       [:h2.section "Corsi: " (count courses)]
       [:ul.channels
        (for [c courses]
          [:li.channel (link-to (channel-path c) (:name c))
           ;[:p.channelInfo (channel/channel-info c)]
           ])]
       [:h2.section "Gruppi: " (count groups)]
       [:ul.channels
        (for [c groups]
          [:li.channel (link-to (channel-path c) (:name c))
           [:p.channelInfo (channel/channel-info c)]
           [:p.channelDescription (:description c)]])]])))

(defpage "/user/new-course" {:keys [field name code year] :as data}
  (layout "Nuovo corso di studi"
    [:h1.section "Nuovo corso di studi"]
    (form-to {:accept-charset "utf-8"} [:post "/user/new-course"]
      [:table
       [:tr [:td.head "Indirizzo: "]
        [:td (drop-down :field (map :name (fetch :fields)) field)]
        (error-cell :field)]
       [:tr [:td.head "Anno: "]
        [:td (drop-down :year (range 1 6) (to-integer year))]
        (error-cell :year)]
       [:tr [:td.head "Codice corso: "]
        [:td (text-field {:placeholder "Codice corso"} :code (or code ""))]
        (error-cell :code)]
       [:tr [:td.head "Nome corso: "]
        [:td (text-field {:placeholder "Nome corso"} :name (or name ""))]
        (error-cell :name)]
       [:tr [:td] [:td (submit-button "Aggiungi")]]])))

;(defn course-channel-description [field name]
;  (str name " (" field ")"))

(defn create-course-channel! [name code]
  (insert! :channels
    {:name name :privacy :public
     ;:description (course-channel-description field name)
     :type :course :code code :created-at (java.util.Date.)}))

(defn create-course! [field name year code]
  (insert! :courses
    {:name name
     :field field
     :year year
     :code code
     :created-by (current-id)
     :created-at (java.util.Date.)}))

(defn valid-course? [{:keys [field name code year]}]
  (let [field (.trim field)
        name (.trim name)
        code (.trim code)]
    (vali/rule (vali/has-value? field)
      [:field "Il campo non deve essere vuoto."])
    (vali/rule (fetch-one :fields :where {:name field})
      [:field "Indirizzo non esistente."])
    (vali/rule (not (fetch-one :courses :where {:field field :name name}))
      [:name "Corso già esistente."])
    (vali/rule (not (str/blank? name))
      [:name "Nome corso non valido"])
    (vali/rule (re-matches #"[a-zA-Z0-9]+" code)
      [:code "Codice non valido"])
    (vali/rule (not (fetch-one :courses :where {:field field :code code}))
      [:code "Corso già esistente."])
    (vali/rule (let [y (Integer/parseInt year)]
                 (and (>= y 1) (<= y 5)))
      [:year "Anno non valido"])
    (not (vali/errors? :field :name :code :year))))

(defpage [:post "/user/new-course"] {:keys [field name code year confirm] :as data}
  (if (current-id)
    (if (valid-course? data)
      (let [field (.trim field)
            name (.trim name)
            code (.trim code)
            old-channel (fetch-one :channels :where {:type :course :code code})]
        (if (and old-channel (not confirm))
          (layout "Conferma nuovo corso"
            [:h1.section "Conferma corso " code " - " name]
            [:p "Esiste già un corso di codice " code ", ed è associato ai seguenti indirizzi di studio:"]
            [:ul
             (for [course (fetch :courses :where {:code code})]
               [:li (:field course) " " (:year course) "°anno - " (:name course)])]
            [:p "Vuoi associare anche \"" field " " year "°anno\" a questo corso?"]
            (form-to {:accept-charset "utf-8" } [:get "/user/new-course"]
              (html (for [[k v] data]
                      [:input {:type :hidden :name k :value v}]))
              (submit-button "Annulla"))
            (form-to {:accept-charset "utf-8" } [:post "/user/new-course"]
              (html (for [[k v] data]
                      [:input {:type :hidden :name k :value v}]))
              [:input {:type :hidden :name :confirm :value :true}]
              (submit-button "Conferma")))
          (let [channel (or old-channel (create-course-channel! name code))]
            (create-course! field name (Integer/parseInt year) code)
            (session/flash-put! (if old-channel "new-course" "new-channel"))
            (resp/redirect (channel-path channel)))))
      (render "/user/new-course" data))
    (render "/permission-denied")))

(defpage "/user/feedback" []
  (layout "Feedbacks"
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

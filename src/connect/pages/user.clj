(ns connect.pages.user
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        noir.core
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [connect.email :as mail]
           [connect.pages.channel :as channel]
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
     [:p (link-to "/user/list" "Elenco utenti")]
     [:p [:a {:href "/user/invite"}
          "Invita i tuoi amici!"] " "
      [:img.edit {:src "/images/add.png"}]]]))

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

(defpage "/user/:num/info" {:keys [num]}
  (layout "Informazioni utente"
    (if (= (session/flash-get) "done") 
      [:p "I dati sono stati modificati"])
    [:h1.section "Informazioni utente"]
    (let [n (to-integer num -1)
          person (fetch-one :people :where {:num n})]
      (if person
        [:span
         (cond (= (:job person) "student")
           (student-info person)
           (= (:job person) "professon")
           [:p "TODO info docente"]
           (= (:job person) "admin")
           [:p "TODO info admin"])
         (when (or (admin? (current-id)) (= (current-id) (:_id person))) 
           [:h2.section (link-to (user-edit-path (:_id person)) "Modifica")])]
        "Utente non esistente."))))

(defpage "/user/list" []
  (let [users (sort-by #(.toLowerCase (or (:lastname %) ""))
                 (sort-by #(.toLowerCase (or (:firstname %) ""))
                   (fetch :people)))]
    (layout "Elenco utenti"
      [:h1.section "Utenti registrati: (" (count users)")"]
      [:p [:a {:href "/user/invite"}
                     [:img.edit {:src "/images/add.png"}]
                     "Invita i tuoi amici!"]]
      [:br]
      (people-table users :lastname true :field true :img true
                    :link (not (nil? (current-id))))
      [:br]
      [:p [:a {:href "/user/invite"}
                     [:img.edit {:src "/images/add.png"}]
                     "Invita i tuoi amici!"]])))

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
    [:td (drop-down :field (sort-by str/upper-case (map :name (fetch :fields)))
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

(defpage "/user/:num/edit" {:keys [num] :as data}
  (let [n (to-integer num -1)
        person (fetch-one :people :where {:num n})]
    (layout "Modifica utente"
      (when (= (session/flash-get) "new-user") 
        (html
          [:h1.section "Benvenuto!"]
          [:p "Grazie per esserti registrato. Ora fai parte di Puiki."]))
      (if person
        (if (or (admin? (current-id)) (= (:_id person) (current-id)))
          (if (= (:job person) "student")
            (student-edit-form person data)
            (professor-edit-form person data))
          "Non hai l'autorizzazione per modificare i dati.")
        "Utente non esistente."))))

(defn valid? [{:keys [num field year firstname lastname]}]
  (vali/rule (vali/has-value? field)
     [:name "Il campo non deve essere vuoto."])
  (vali/rule (fetch-one :fields :where {:name field})
     [:name "Indirizzo non esistente."])
  (vali/rule (fetch-one :people :where {:num (to-integer num -1)})
     [:id "Matricola non valida."]) ;; Non visualizzato
  (vali/rule (vali/has-value? firstname)
     [:firstname "Il campo non deve essere vuoto."])
  (vali/rule (vali/has-value? lastname)
     [:lastname "Il campo non deve essere vuoto."])
  (vali/rule (let [y (Integer/parseInt year)]
               (and (>= y 2007) (<= y 2012)))
     [:year "Anno di immatricolazione non valido."])
  (not (vali/errors? :field :year :firstname :lastname :id)))

(defpage [:post "/user/:num/edit"] {:keys [num year field firstname lastname] :as person}
  (if (not (valid? person))
    (render "/user/:num/edit" person)
    (let [y (Integer/parseInt year)
          n (to-integer num -1)
          p (fetch-one :people :where {:num n})
          channel (fetch-one :channels :where {:field field :year (- 2012 y)})]
      (when (or (admin? (current-id)) (= (current-id) (:_id p)))
        (update! :people {:_id (:_id p)}
          {:$set {:year y :field field :firstname firstname :lastname lastname}})
        (let [codes (map :code (fetch :courses :where {:field field :year (- 2012 y)}))
              channels (map #(:_id (fetch-one :channels :where {:code %})) codes)]
          (dorun
            (map #(update! :people {:_id (current-id)}
                           {:$addToSet {:follows %}})
                 channels)))
        (when channel (follow-channel (:_id channel) (:_id p)))
        (session/flash-put! "done")
        (resp/redirect (user-info-path (:_id p)))))))

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
        new-comments (filter #(= (:action %) "new-comment") news)
        new-files (filter #(= (:action %) "new-file") news)]
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
         (html [:h2.section "Nuovi commenti ai post:"]
           (channel/post-links
             (map #(fetch-one :posts :where {:_id (:post %)}) new-comments) :show-removed)))
       (when (not (empty? new-files))
         (html [:h2.section "Nuovi files caricati:"]
           (let [groups (group-by :channel new-files)]
             (for [[channel files] groups]
               [:div.section
                [:h3.section (:name (fetch-one :channels :where {:_id channel}))]
                [:div.section
                 (channel/files-list (map #(fetch-one :files :where {:channel (str (:channel %))
                                                                     :filename (:filename %)})
                                          files))]]))))]
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

(defpage "/user/feedback" {:keys [text]}
  (layout "Feedbacks"
    [:h1.section "Feedback"]
    [:p "Se vuoi riportare un errore, un malfunzionamento, un suggerimento qualsiasi "
     "scrivi nella casella sottostante, oppure mandami un "
     (link-to "mailto:giraud.federico@gmail.com" "email") "."]
    [:p "Grazie per il tuo aiuto!"]
    (form-to {:accept-charset "utf-8"} [:post "/user/feedback"]
      (text-area {:class :postComment :rows 10 :placeholder "Vorrei che..."} :text text)
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

;(def js-search 
;  "$('#loader').css('display', 'inline');
;  $.post('/user/invite-search', {firstname: $('#firstname').val(), lastname: $('#lastname').val()},
;     function(content) {$('#searchResult').html(content);
;  $('#loader').css('display', 'none');});")

(defpartial invite-email [user-firstname user-lastname firstname lastname & [msg]]
  [:img {:src "http://www.puiki.it/images/logo.png" :style "float: right; padding: 10px"}]
  (when (not (empty? firstname))
    [:p "Ciao " firstname ","])
  [:p user-firstname " " user-lastname " ti invita a far parte del progetto "
   [:a {:href "http://www.puiki.it"} "Puiki"] "!"]
  [:p "Puiki è un sito web dedicato agli studenti del Politecnico di Torino, dove puoi condividere files, "
   "fare domande, scrivere e cercare informazioni sui tuoi corsi di studio e altro ancora."]
  [:p "L'iscrizione è rapidissima! E' sufficiente cliccare " [:a {:href "http://www.puiki.it/register"} "qui"]
   " e in pochi minuti potrai accedere a tutti i contenuti di Puiki."]
  (when (and msg (not (str/blank? msg)))
    (html
      [:h2.section user-firstname " scrive:"]
      [:p msg])))

(defn format-names [s]
  (apply str
    (interpose " "
      (map #(str (first (.toUpperCase %))
                 (subs (.toLowerCase %) 1))
           (re-seq #"[^ ]+" s)))))

(def sorted-map-names
  (into (sorted-map-by (fn [key1 key2] (compare (str key2) (str key1))))
      {:lastname 1 :firstname 1}))

(defpage "/user/invite" {:keys [firstname lastname id msg email] :as data}
  (layout "Invita un amico"
    [:h1.section "Invita un amico"]
    [:h2.section "Cerca un amico del Politecnico e invitalo ad iscriversi"]
    [:p "Puiki ha bisogno del tuo contributo per farsi conoscere e permettere così di "
     "arricchirsi del contributo di più persone."]
    [:p "Cerca i tuoi amici del Politecnico e Puiki gli invierà un email di invito a tuo nome."]
    [:div.section
     (form-to {:accept-charset "utf-8"} [:get "/user/invite"]
       [:table
         [:tr [:td.head "Nome:"]    [:td (text-field {:placeholder "Nome"} :firstname firstname)]]
         [:tr [:td.head "Cognome:"] [:td (text-field {:placeholder "Congnome"} :lastname lastname)]
          [:td (submit-button "Cerca")]]])]
    [:div.section
     [:p "Oppure inserisci la sua email:"]
     (form-to {:accept-charset "utf-8"} [:get "/user/invite"]
        [:table
         [:tr [:td.head "Email:"] [:td (text-field :email email)] [:td (submit-button "Avanti")]]])]
    (when (or (not (str/blank? firstname))
              (not (str/blank? lastname)))
      (if (or (> (count firstname) 3)
              (> (count lastname) 3))
        (let [q1 (str "^" firstname ".*")
              q2 (str "^" lastname ".*")
              res (fetch :students :where {:firstname {:$regex q1 :$options "i"}
                                           :lastname {:$regex q2 :$options "i"}
                                           ;:cds {:$regex ".*1T3" :$options "i"}
                                           }
                         :sort sorted-map-names)
              res2 (take 20 res)
              admin? (admin? (current-id))]
          (html
            [:h2.section "Risultati ricerca: " (count res2) " di " (count res)]
            [:div.section
             (for [s res2]
               [:p [:a {:href (str (encode-url "/user/invite" {:firstname firstname :lastname lastname
                                                          :id (:_id s)})
                                   "#preview")}
                    (format-names (:lastname s)) " " (format-names (:firstname s))]
                 " " (:cds s)
                (when admin? (str " " (:code s)))])]))
        (html
          [:h2.section "Risultati ricerca: 0"]
          [:div.section
           [:p "Ricerca non valida, inserisci parametri più specifici."]])))
    (when (or (not (str/blank? id)) (not (str/blank? email)))
      (let [st (if (str/blank? id)
                 {:firstname "" :lastname ""}
                 (fetch-one :students :where {:_id (obj-id id)}))
            user (fetch-one :people :where {:_id (current-id)})]
        (html
          [:h2.section {:id "preview"}
           "Anteprima invito a "
           (if (str/blank? id)
             email
             (str (format-names (:firstname st)) " " (format-names (:lastname st))))]
          [:div.section
           (invite-email (:firstname user) (:lastname user)
                         (format-names (:firstname st)) (format-names (:lastname st)))
           [:h3.section (:firstname user) " scrive (opzionale):"]
           (form-to {:accept-charset "utf-8"} [:get "/user/invite-send"]
             (text-area {:class :postComment :rows 4 :maxlength 1000 :style "width: 500px"}
                :msg (or msg "Io mi sono già iscritto!"))
             (html (for [[k v] data]
                     [:input {:type :hidden :name k :value v}]))
             [:div 
              [:img.edit {:src "/images/send_mail.png"}]
              (submit-button "Invia!")])])))))

;(dorun
;  (for [[m d] (read-string (slurp "/home/federico/Archivio/db-studenti-2"))]
;    (insert! :students (merge d {:code (str "s" (:code d))}))))

(defn valid-mail [mail]
  (when mail 
    (re-matches #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$" mail)))

(defpage "/user/invite-send" {:keys [id msg email] :as data}
  (let [st (when (not (str/blank? id))
             (fetch-one :students :where {:_id (obj-id id)}))
        user (fetch-one :people :where {:_id (current-id)})
        final-email (valid-mail (if st
                                  (str (:code st) "@studenti.polito.it")
                                  email))]
    (if (and st (fetch-one :people :where {:_id (:code st)}))
      (layout "Utente già registrato"
        [:h1.section "L'utente scelto è già registrato!"]
        [:h2.section "Invita altri amici"]
        [:p "Cerca altri amici da invitare, più siamo e più possiamo contribuire. "
           [:a {:href "/user/invite"} "Continua!"]])
      (if final-email
        (if (or (> (count (:invites user)) 30)
               (some #(= % final-email) (:invites user)))
          (if (> (count (:invites user)) 30)
            (layout "Limite inviti raggiunto"
              [:h1.section "Limite inviti raggiunto"]
              [:p "Hai raggiunto il limite di inviti che puoi inviare. Grazie!"])
            (layout "Già invitato"
              [:h1.section "Persona già invitata"]
              [:p "Hai già invitato questa persona."]))
          (do
            (update! :people {:_id (current-id)}
               {:$push {:invites final-email}})
            (future
              (try
                (mail/send-mail final-email ;"giraud.federico@gmail.com"
                   (str (:firstname user) " " (:lastname user) " ti invita ad iscriverti a Puiki.it")
                   (html [:h1 "Benvenuto"]
                     (invite-email (:firstname user) (:lastname user)
                       (format-names (or (:firstname st) "")) (format-names (or (:lastname st) ""))
                       msg)))
                (catch Exception e
                  (println "Email error." (str e)))))
            (layout "Email inviata"
              [:h1.section "Grazie!"]
              (if (str/blank? id)
                [:p "L'invito è stato inviato all'indirizzo " final-email "."]
                [:p "L'invito è stato inviato all'indirizzo di " (format-names (or (:firstname st) "")) "."])
              [:h2.section "Invita altri amici"]
              [:p "Cerca altri amici da invitare, più siamo e più possiamo contribuire. "
               [:a {:href "/user/invite"} "Continua!"]])))
         (layout "Errore invio"
          [:h1.section "Errore invio email"]
          (if (str/blank? id)
            [:p "L'email specificata non è valida."]
            [:p "L'utente cercato non è nel database di Puiki.it"]))))))
  
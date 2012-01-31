(ns connect.pages.home
  (:use connect.pages.layout
        connect.pages.utils
        connect.fb
        connect.db
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [connect.pages.channel :as channel]
           [noir.session :as session]
           [noir.validation :as vali]
           [noir.response :as resp]
   [noir.util.test :as test]))

 (defpartial info-section []
  (let [div-prop {:style "text-align: justify; margin-right: 20px;"}]
    [:span
     [:h1.section "Benvenuto!" [:div.like_button (like-button "/")]]
     [:div.section div-prop
      [:p "Benvenuto su Puiki, il sito creato per gli studenti del Politecnico di Torino, "
       "dove fare domande e trovare informazioni riguardanti i corsi, condividere appunti e altro ancora."]]
     [:h2.section "Condividi ciò che sai e chiedi cosa vuoi sapere:"]
     [:div.section div-prop
      [:img.right {:src "/images/logo.png" :style "padding-left: 10px"}]
      [:p "Il sito ha una pagina dedicata ad ogni " (link-to "/channel/list" "materia")
       ", in cui puoi porre le tue domande e condividere ciò che sai."
       " Ogni studente iscritto può aggiungere le sue domande (" (link-to "/post/4ea28778e4b0b301111bb242" "un esercizio")
       ", un dubbio su un argomento della lezione...) nella pagina dedicata alla materia e una notifica arriverà a tutti gli studenti che seguono il tuo stesso corso, i quali potranno rispondere."
       " Le varie risposte vengono poi ordinate in base ai giudizi degli studenti stessi, così che la risposta ritenuta migliore sia visualizzata per prima."]
      [:p "Puoi anche condividere ciò che sai e contribuire a raccogliere informazioni sui corsi, tramite la sezione wiki, dove puoi creare nuove pagine o modificare quelle esistenti, aggiungendo informazioni utili ad altri studenti."
       " Vedi una " (link-to "http://www.puiki.it/post/4f0c3627e4b056ec797378a3" "pagina di esempio") "."]
      [:p "Il contenuto delle pagine, delle domande e delle risposte viene indicizzato per permettere una "
       (link-to "/search?text=java+public" "rapida ricerca")
       " e per trovare subito le informazioni che cerchi."]]
     [:h2.section "Iscrizione:"]
     [:div.section div-prop
      [:p "Se sei uno studente del Politecnico di Torino l'iscrizione è rapidissima, è sufficiente cliccare "
       (link-to "/register" "qui") " e in un attimo sarai in grado di aggiungere le tue domande, provare a rispondere ai quesiti già presenti e creare nuove pagine wiki."]
      [:p "Per ogni informazione/suggerimento scrivi a "
       (link-to "mailto:giraud.federico@gmail.com" "giraud.federico@gmail.com")
       " o utilizza il pulsante Feedback in alto."]]]))

(defpage "/" []
  (layout "Puiki.it"
    (info-section)
    [:h2.lastQuestions [:img.middle {:src "/images/question-big.png"}] " Domande più recenti:"]
    (let [posts (fetch :posts :where {:removed {:$ne true} :type :question}
                       :sort {:created-at -1} :limit 5)]
      [:div.section (channel/post-links posts)])
    [:h2.lastPages [:img.middle {:src "/images/wiki.png"}] " Ultime pagine wiki:"]
    (let [pages (concat
                  (fetch :posts :where {:removed {:$ne true} :type :normal}
                         :sort {:modified-at -1} :limit 5)
                  (fetch :posts :where {:removed {:$ne true} :type :normal}
                         :sort {:created-at -1} :limit 5))
          ordered (sort-by #(or (:modified-at %) (:created-at %)) (distinct pages))]
      [:div.section
       (for [page (take 5 (reverse (sort-by #(or (:modified-at %) (:created-at %)) ordered)))]
         (let [channel (fetch-one :channels :where {:_id (:channel page)})]
           [:p [:a {:href (post-path page)}
                [:img.edit {:src "/images/page.png"}] [:b (:title page)]]
            " - " [:a {:href (channel-path channel)} (:name channel)]]))])
    [:h2.lastPages [:img.middle {:src "/images/files.png"}] " Ultimi files (beta)"]
    (let [files (if (current-id)
                  (fetch :files :sort {:created-at -1} :limit 5)
                  (fetch :files :where {:privacy "Pubblico"}
                         :sort {:created-at -1} :limit 5))]
      [:div.section
       (for [file files]
         (let [channel (fetch-one :channels :where {:_id (obj-id (:channel file))})]
           (html
             [:a {:href (file-path (:channel file) (:filename file))}
              [:img.edit {:src "/images/box.png"}] [:b (:filename file)]]
             " - " [:a {:href (channel-path channel)} (:name channel)]
             " - " (:category file)
             (when (current-id) (str " (" (:privacy file) ")")) [:br])))])))
          
(defpage [:post "/"] {:keys [signed_request ref fb_source] :as data}
  ;(println (pr-str signed_request ref fb_source))
  (let [fb-user-id (get (decode-string-request signed_request) "user_id")]
    (when fb-user-id
      (session/put! :fb-id fb-user-id)
      (when (current-id)  ;; Memorizza l'id di facebook
        (update! :people {:_id (current-id)}
          {:$set {:fb-id fb-user-id}}))))      
  (render "/"))

(ns connect.pages.home
  (:use connect.pages.layout
        connect.pages.utils
        connect.fb
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
     [:h1.section "Benvenuto!"]
     [:div.section div-prop
      [:p "Benvenuto su PoliConnect, il sito creato per gli studenti del Politecnico di Torino, "
       "dove fare domande e trovare informazioni riguardanti i corsi e non solo."]]
     [:h2.section "Come funziona:"]
     [:div.section div-prop
      [:p "Il sito è diviso in " (link-to "/channel/list" "canali")
       ", ognuno riguardante un corso di studi (ad esempio Ingegneri Informatici iscritti al "
       (link-to "/channel/4ea283f4e4b0b301111bb1e2" "terzo anno")
       ") oppure una " (link-to "/channel/4ea2844fe4b0b301111bb1f3" "materia di studio")
       ". Ogni studente iscritto può aggiungere le sue domande (" (link-to "/post/4ea28778e4b0b301111bb242" "un esercizio")
       ", un dubbio su un argomento della lezione...) nel canale adeguato e una notifica arriverà a tutti gli studenti che seguono il canale, i quali potranno rispondere."
       " Le varie risposte vengono poi ordinate in base ai giudizi degli studenti stessi, così che la risposta ritenuta migliore sia visualizzata per prima. Ad ogni nuova risposta alle domande che hai effettuato ti arriverà una notifica."]
      [:p "Il contenuto delle domande e delle risposte viene indicizzato per permettere una "
       (link-to "/search?text=java+public" "rapida ricerca")
       " e per trovare subito le informazioni che cerchi."]]
     [:h2.section "Iscrizione:"]
     [:div.section div-prop
      [:p "Se sei uno studente del Politecnico di Torino l'iscrizione è rapidissima, è sufficiente cliccare "
       (link-to "/register" "qui") " e in un attimo sarai in grado di aggiungere le tue domande o provare a rispondere ai quesiti già presenti."]
      [:p "Per ogni informazione/suggerimento scrivi a "
       (link-to "mailto:giraud.federico@gmail.com" "giraud.federico@gmail.com")
       " o utilizza il pulsante Feedback in alto."]]
     [:h2.section "Versione beta:"]
     [:div.section div-prop
      [:p "Il sito è in fase di test, abbiamo quindi bisogno di studenti che abbiano voglia di provare ad utilizzarlo inserendo nuovi contenuti."
       "	Ora è disponibile solo un funzionamento di base ma molto sarà aggiunto, seguici!"]]]))

(defpage "/" []
  (layout "PoliConnect"
    (info-section)
    [:h2.section [:img.middle {:src "/images/question.png"}] "Domande più recenti:"]
    (let [posts (fetch :posts :where {:removed {:$ne true} :type :question}
                       :sort {:created-at -1} :limit 5)]
      (channel/post-links posts))
    [:h2.section [:img.middle {:src "/images/page.png"}] "Pagine più recenti:"]
    (let [posts (fetch :posts :where {:removed {:$ne true} :type :normal}
                       :sort {:created-at -1} :limit 5)]
      (channel/post-links posts))))

(defpage [:post "/"] {:keys [signed_request ref fb_source] :as data}
  ;(println (pr-str signed_request ref fb_source))
  (let [fb-user-id (get (decode-string-request signed_request) "user_id")]
    (when fb-user-id
      (session/put! :fb-id fb-user-id)
      (when (current-id)  ;; Memorizza l'id di facebook
        (update! :people {:_id (current-id)}
          {:$set {:fb-id fb-user-id}}))))      
  (render "/"))

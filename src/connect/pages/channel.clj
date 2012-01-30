(ns connect.pages.channel
  (:use connect.pages.layout
        connect.pages.utils
        connect.pages.post
        connect.db
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
  (:import [java.net URLEncoder URLDecoder])
  (:require [connect.logs :as logs]
            [clojure.java.io :as io]
            [connect.s3 :as s3]
            [clojure.contrib.string :as str]
            [noir.server :as server]
            [noir.validation :as vali]
            [noir.session :as session]
            [noir.response :as resp]))

(def channel-types
  {"group" "Gruppo",
   "field" "Indirizzo di studio", "course" "Corso"})

(def privacy-options
  {"public" "Pubblico" "private" "Privato"})

(def preferred-channels-number 5)

(defpartial channel-info [ch]
  "Tipo canale: " (channel-types (:type ch))
  " (" (privacy-options (or (:privacy ch) "public")) "), "
  "contiene " (or (:posts ch) 0) " post ed è seguito da "
  (or (count-followers (:_id ch)) 0) " persone.")

(defpartial channel-list-link [c & {:keys [name info description]}]
  [:li.channel
   [:a.channel {:href (channel-path c)}
    [:img.year {:src "/images/users.png"}] (or name (:name c))]
   (when info [:p.channelInfo (channel-info c)])
   (when description
     [:p.channelDescription (:description c)])])

(def cardinali 
  {1 "Primo"  2 "Secondo"  3 "Terzo"  4 "Quarto"  5 "Quinto"})

(defpartial field-channels [f]
  (let [channels (fetch :channels :where {:type :field :field (:name f)}
                        :sort {:year 1})]
     (if (not (empty? channels))
       [:ul.years
        (for [c channels]
          [:li.year
           [:a.year {:onClick (js-toggle-anim "#channels_" (:year c) "_" (:_id f))}
              [:img.year {:src "/images/users.png"}] (cardinali (:year c)) " anno"]
           [:ul.channels {:id (str "channels_" (:year c) "_" (:_id f)) :style "display: none"}
            [:li.channel [:a.channel {:href (channel-path c)} "Canale dedicato a " (:name c)]]
            (for [course (fetch :courses :where {:field (:name f) :year (:year c)}
                                :sort {:name 1})]
              (if-let [ch (fetch-one :channels :where {:code (:code course)})]
                [:li.channel [:a.channel {:href (channel-path ch)} (:name ch)]]))]])]
       "Nessun canale presente.")))

(defpage "/channel/list" []
  (let [last-channels (when (current-id)
                        (:last-channels (fetch-one :people :where {:_id (current-id)})))]
    (layout "Tutti i canali"
      [:div.like_button (like-button "/channel/list")]
      (when (not (empty? last-channels))
        (html
          [:h1.section "Preferiti:"]
          [:ul.channels
           (for [c (sort-by :name (map #(fetch-one :channels :where {:_id %}) last-channels))]
             [:li.channel
              [:a.channel {:href (channel-path c)}
                [:img.channel {:src "/images/users.png"}] (:name c)]])]
          [:br]))
      [:h1.section "Indirizzi di studio:"]
      [:ul.fields
       (for [f (fetch :fields :sort {:name 1})]
         [:li.field 
          [:h2.section.link {:onClick (js-toggle-anim "#descr_" (:_id f))}
           [:img.icon {:src "/images/phd.png"}] (:name f)]
          [:div {:id (str "descr_" (:_id f)) :style "display: none"}
           (field-channels f)]])]
      [:h1.section "Gruppi:"]
      [:ul.channels
       (for [c (fetch :channels :where {:type "group"} :sort {:name 1})]
         (channel-list-link c :description true))])))

(defpartial followers [channel]
  (let [limit 10
        flw (shuffle (fetch :people :where {:follows (:_id channel)}))
        count (count flw)]
    [:div.sideBarSection
     [:h2.section "Followers: ("
      (min limit count) (when (> count limit) (str " di " count)) ")"]
     (people-table (sort-by :lastname (sort-by :firstname (take limit flw)))
                   :lastname (current-id))
     (when (> count limit)
       [:p "..."])]))

(defpartial add-post [channel]
  [:div.sideBarSection
   [:h2.section "Modifica:"]
   [:p [:a {:href (encode-url "/edit/new-question" {:channel (:_id channel)})}
        [:img.middle {:src "/images/question.png"}] "Nuova domanda"]]
   [:p [:a {:href (encode-url "/edit/new-page" {:channel (:_id channel)})}
        [:img.middle {:src "/images/page.png"}] "Nuova pagina"]]
   [:p [:a {:href (encode-url "/edit/upload" {:channel (:_id channel)})}
        [:img.middle {:src "/images/upload.png"}] "Carica file (beta)"]]
  [:h2.section "Cerca:"]
  (form-to [:get "/search"]
    [:input {:type :hidden :name :channel :value (:_id channel)}]
    (text-field {:class :channelSearchText :placeholder "Testo ricerca"} :text)
    (submit-button {:class "channelSearch"} "Cerca"))
  [:h2.section "Condividi:"]
  (like-button (channel-path channel))])

;; TODO: dividere in question-links e pages-link
(defpartial post-links [posts & [show-removed]]
  [:table.postLink
   (for [post posts]
     (when (or show-removed (not (:removed post)))
       (let [vote (or (:vtotal post) 0)]
         (html
           [:tr
            [:td.postLinkVote {:title "Voto"}
             (str (when (> vote 0) "+") (when (= vote 0) " ") vote)]
            [:td.postLinkSpace]
            (if (= (:type post) "question")
              [:td.postLinkAnswers {:title "Risposte"}
               (or (:answers post) 0)]
              [:td.postLinkAnswers {:title "Commenti"}
               (count (:comments post))])
            [:td.postLinkSpace]
            [:td.postLinkTitle 
             (link-to {:class :postLinkTitle} (post-path post) (:title post))]
            [:td.postLinkDate (format-timestamp-relative
                                (if (= (:type post) "normal")
                                  (or (:modified-at post) (:created-at post))
                                  (:created-at post)))]]
           [:tr.postLinkSpace]))))])

;; TODO: solo i link agli indirizzi? Dividere la funzione in due?
(defpartial channel-description [ch]
  (if (= (:type ch) "course")
    (html [:h2.section "Il corso si applica agli studenti di:"]
     [:ul.years
      (for [course (fetch :courses :where {:code (:code ch)})]
        (if-let [channel (fetch-one :channels :where {:type :field :field (:field course) :year (:year course)})]
          [:li.year [:img.year {:src "/images/users.png"}]
           (link-to (channel-path channel) (:field course) " " (:year course) "°anno")]
          [:li.year [:img.year {:src "/images/users.png"}]
           (:field course) " " (:year course) "°anno"]))])
    [:p (:description ch)]))

(defpartial channel-follow-buttons [c action & {:keys [only-button]}]
  (if (= action 'add)
    [:div.follow_button
     [:img.follow_button {:src "/images/buttons/segui.png"
                          :style "opacity: 0.7"
                          :onClick (js-post "/channel/follow" (:_id c)
                                            {:channel-id (str (:_id c)) :action "add" :only-button only-button})}]
     [:br] [:p.follow_descr {:style "opacity: 0"} "Clicca per ricevere gli aggiornamenti."]]
    [:div.follow_button 
     [:img.follow_button {:src "/images/buttons/following.png"
                          :style "opacity: 0.7"
                          :onClick (js-post "/channel/follow" (:_id c)
                                            {:channel-id (str (:_id c)) :action "remove" :only-button only-button})}]
      [:br] [:p.follow_descr {:style "opacity: 0"} "Clicca per non ricevere più gli aggiornamenti."]])
  [:script {:type "text/javascript"}
   "$('img.follow_button').hover(
       function() { $(this).stop().animate({ 'opacity': 1 }, 'fast');
                    $('p.follow_descr').stop().animate({ 'opacity': 1 }, 'fast');},
       function() { $(this).stop().animate({ 'opacity': 0.7 }, 'fast');
                    $('p.follow_descr').stop().animate({ 'opacity': 0 }, 'fast');}
);"])

;; TODO: togliere only-button? Semplificare?
(defpage [:post "/channel/follow"] {:keys [channel-id action only-button]}
  (let [id (obj-id channel-id)]
    (when (current-id)
      (if (= action "add")
        (update! :people {:_id (current-id)}
          {:$push {:follows id}})
        (update! :people {:_id (current-id)}
          {:$pull {:follows id}})))
    (let [c (fetch-one :channels :where {:_id id})
          follows (into #{} (:follows (fetch-one :people :where {:_id (current-id)})))]
      (if (= only-button "true")
        (channel-follow-buttons c (if (get follows id) 'remove 'add) :only-button true)
        (channel-list-link c :follows follows)))))

;; Mantiene un vettore ordinato degli ultimi canali visitati,
;; se la lunghezza va oltre preferred-channels-number elimina
;; quello visitato meno recentemente.
(defn store-preferred-channel [ch]
  (let [person-id (current-id)]
    (when person-id
      (let [ch-id (:_id ch)
            last-channels (:last-channels (fetch-one :people :where {:_id person-id}))
            new (not (some #(= % ch-id) last-channels))]
        (if new
          (when (>= (count last-channels) preferred-channels-number)
            (update! :people {:_id person-id}
               {:$pop {:last-channels -1}}))
          (update! :people {:_id person-id}
               {:$pull {:last-channels ch-id}}))
        (update! :people {:_id person-id}
           {:$push {:last-channels ch-id}})))))

(defpage "/channel/:id" {:keys [id show]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})
        follows (when (current-id) ;; TODO: serve?
                  (into #{} (:follows (fetch-one :people :where {:_id (current-id)}))))]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (store-preferred-channel ch)
        (when (not logs/*bot*)
          (update! :channels {:_id id} {:$inc {:views 1}}))
        (layout (:name ch)
          (when (= (session/flash-get) "new-channel") 
            [:p "Nuovo canale creato."])
          (when (= (session/flash-get) "new-course") 
            [:p "Nuovo corso creato."])
          (when (current-id)
            [:span {:id id}
             (channel-follow-buttons ch (if (get follows id) 'remove 'add) :only-button true)])
          [:h1.section (:name ch)]
          (channel-description ch)
          ;[:p (channel-info ch)]
          ;[:p "Canale creato il: " (format-timestamp (:created-at ch))]
          (when (= (:type ch) "field")
            (html [:h2.section "Corsi:"]
              [:ul.channels 
               (for [course (fetch :courses :where {:field (:field ch) :year (:year ch)}
                                   :sort {:name 1})]
                 (if-let [c (fetch-one :channels :where {:code (:code course)})]
                   [:li.channel
                    (link-to (channel-path c) (:name c))]))]
              (link-to (encode-url "/user/new-course" {:field (:field ch) :year (:year ch)})
                       "Crea nuovo corso")))
          ;[:p (link-to "/user/following" "Canali seguiti")]
          [:br]
          (let [pages (fetch :posts :where {:channel id :type "normal" :removed {:$ne true}}
                             :sort {:title 1})
                questions (fetch :posts :where {:channel id :type "question" :removed {:$ne true}}
                                 :sort {:created-at -1} :limit (if (= show "all") 0 5))
                files (fetch :files :where {:channel (str id)} :sort {:filename 1})]
            (html
              [:h2.lastQuestions [:img.middle {:src "/images/question-big.png"}] " Ultime domande"]
              [:div.section
               (if (empty? questions)
                 [:p "Non ci sono ancora domande in questo canale."]
                 (html 
                   (post-links questions)
                   [:p.right (link-to (str (channel-path ch) "?show=all") "Mostra tutto")]
                   ))]
              [:h2.lastPages [:img.middle {:src "/images/wiki.png"}] " Pagine wiki"]
              [:div.section
               (if (empty? pages)
                 [:p "Non ci sono ancora pagine wiki in questo canale."]
                 (for [page pages]
                   [:p [:a {:href (post-path page)}
                        [:img.edit {:src "/images/page.png"}] [:b (:title page)]]]))]
              [:h2.lastPages [:img.middle {:src "/images/files.png"}] " Files (beta)"]
              [:div.section
               (if (empty? files)
                 [:p "Non ci sono ancora files condivisi in questo canale."]
                 (for [file files]
                   (if (and (= (:privacy file) "Privato")
                            (not (current-id)))
                     [:span [:img.edit {:src "/images/box.png"}] 
                      "File privato, esegui il login per accedere." [:br]]
                     (html
                       [:a {:href (file-path id (:filename file))}
                        [:img.edit {:src "/images/box.png"}] [:b (:filename file)]]
                       " - " (:category file) 
                       (when (current-id) (str " (" (:privacy file) ")")) [:br]))))]))
          [:p [:img.edit {:src "/images/users.png" :alt "Vis" :title "Visualizzazzioni"}]
              "Canale visualizzato " (+ 1 (or (:views ch) 0)) " volte."])))))


(def auth {:secret-key "Y69xCpsGpStpnZsaBsjHMM5aUepNmdRRwThNBezE" :access-key "AKIAJGKXPJQMXBDLLF2A"})

(defn get-bucket-name []
  (:bucket (fetch-one :admin :where {:_id "s3"})))

(def expire-minutes 1)

(def file-categories ["Appunti" "Esercizi" "Laboratori" "Temi d'esame" "Vario"])

(def size-limit-mb 30) ;; Limite dimensione file in megabyte

(defpage "/edit/upload" {:keys [channel]}
  (let [ch (fetch-one :channels :where {:_id (obj-id channel)})]
    (if ch
      (layout "Condividi un file"
        [:h1.section "Carica un file (beta)"]
        [:h2.section "Condividi un file con chiunque o con gli altri studenti"]
        (form-to {:enctype "multipart/form-data"} [:post "/edit/upload"]
          [:input {:type :hidden :name :channel :value channel}]
          [:p (file-upload :file) " La dimensione massima è di " size-limit-mb "Mb."]
          [:p "Descrizione file: "
           (text-area {:class :postComment :rows 3} :description)]
          [:p "Categoria:" (drop-down :category file-categories "Vario")
           " Visibilità:" (drop-down :privacy '["Pubblico" "Privato"])]
          [:p "I file pubblici sono accessibili a chiunque, mentre quelli privati solo agli utenti iscritti "
           " e quindi solo agli studenti."]
          [:p "Canale: " (link-to (channel-path ch) (:name ch))]
          (submit-button "Carica")
          [:p "Il caricamento potrebbe richiedere molto tempo per file di grandi dimensioni, attendi."]))
      (layout "Errore" "Canale non valido."))))

(defn valid-file-descripion? [{:keys [description channel category privacy]}]
  (vali/rule (fetch-one :channels :where {:_id (obj-id channel)})
    [:channel "Canale non valido"])
  (vali/rule (some #(= category %) file-categories)
    [:category "Categoria non valida"])
  (vali/rule (some #(= privacy %) '["Pubblico" "Privato"])
    [:privacy "Visibilità non valida"])
  (not (vali/errors? :name :channel :category :privacy)))

;(defpage [:post "/edit/upload-select-file"] {:keys [name description channel category] :as data}
;  (if (valid-file-descripion? data)
;    (layout "Scegli il file"
;       (form-to {:enctype "multipart/form-data"} [:post "/edit/upload-end"]
;         [:input {:type :hidden :name :name :value name}]
;         [:input {:type :hidden :name :description :value description}]
;         [:input {:type :hidden :name :channel :value channel}]
;         [:input {:type :hidden :name :category :value category}]
;         [:p "File " name ", categoria " category]
;         [:p "Descrizione file: " (if (str/blank? description) "nessuna." description)]
;         [:p "File: " (file-upload :file)]
;         [:p "La dimensione massima è di " size-limit-mb "Mb."]
;         (submit-button "Carica")))
;    (render "/edit/upload" data)))

(defn valid-file? [file channel]
  (let [size (to-integer (:size file))]
    (vali/rule (not (fetch-one :files :where {:channel channel :filename (:filename file)}))
      [:file "File già esistente"])
    (vali/rule (> size 0)
      [:file "Errore caricamento file."])
    (vali/rule (< size (* size-limit-mb (Math/pow 1024 2)))
      [:file "File troppo grande."]))
  (not (vali/errors? :file)))

(defn upload-channel-file! [file description channel category privacy]
  (let [obj-name (str channel "/" (:filename file))
        bucket (get-bucket-name)
        ret (try
              (s3/with-s3 auth
                (s3/put-file! (:tempfile file) bucket
                  :name obj-name
                  :mime (:content-type file)
                  :public (= privacy "Pubblico")))
              (catch Exception exc
                (println (pr-str exc))
                nil))]
    (when ret
      (insert! :files
        {:channel channel :filename (:filename file) :description description
         :category category :obj-name obj-name :size (to-integer (:size file))
         :privacy privacy :content-type (:content-type file) :bucket bucket
         :created-at (java.util.Date.) :author (current-id)}))))

(defpage [:post "/edit/upload"] {:keys [file description channel category privacy] :as data}
  (if (and (valid-file-descripion? data) (valid-file? file channel))
    (let [ret (upload-channel-file! file description channel category privacy)
          ch (fetch-one :channels :where {:_id (obj-id channel)})]
      (.delete (:tempfile file))
      (if ret
        (layout "File caricato"
           [:h1.section "File caricato"]
           [:h2.section "Grazie per aver condiviso un file!"]
           [:p "Torna alla pagina di " (link-to (channel-path ch) (:name ch)) "."])
        (layout "Errore"
          [:h1.section "Errore"]
          [:h2.section "Si è verificato un errore"]
          (for [[field errors] @vali/*errors*]
            (for [error errors]
              [:p error]))))) ;; TODO: controllo errore
    (do
      (.delete (:tempfile file))
      (layout "Errore"
        [:h1.section "Errore"]
        [:h2.section "Si è verificato un errore"] ;; TODO: controllo errore
        (for [[field errors] @vali/*errors*]
          (for [error errors]
            [:p error]))))))

(defn channel-file-redirect [file]
  (if file
    (let [url (if (= (:privacy file) "Pubblico")
                (str "http://" (get-bucket-name) "/" (:obj-name file))
                (s3/with-s3 auth
                  (s3/get-expiring-url (:obj-name file) (get-bucket-name) expire-minutes :virtual true)))] 
      {:status 302
       :headers {"Location" url}})
    (resp/redirect "/not-found")))

(defpage "/channel/:id/files" {:keys [id filename action]}
  (let [dec-filename (URLDecoder/decode filename)
        file (fetch-one :files :where {:channel id :filename dec-filename})]
    (if file
      (if (or (= (:privacy file) "Pubblico")
              (current-id))
        (if (= action "info")
          (layout "ok"
             [:h1.section dec-filename]
             (link-to (file-path id filename :action 'open) "Apri"))
          (do
            (when (not logs/*bot*)
              (update! :files {:channel id :filename dec-filename}
                       {:$inc {:views 1}}))
            (channel-file-redirect file)))
        (layout "Effettua il login"
          [:h1.section "Permessi insufficienti"]
          [:p "Effettua il login per poter accedere al file."]))
      (render "/not-found"))))

;(defpage "/upload" []
;  (layout "Condividi un file"
;    [:h1.section "Carica un file"]
;    [:h2.section "Condividi un file con chiunque o con gli altri studenti"]
;    (form-to {:enctype "multipart/form-data"} [:post "/upload"]
;      [:input {:type :file :name :file}]
;      (submit-button "Carica"))))

;(defpage [:post "/upload"] {:keys [file]}
;  (println file)
;  (let [ret (if (not (= "0" (:size file)))
;              (do
;                (s3/with-s3 auth
;                   (s3/put-file! (:tempfile file) "PoliConnect"
;                       :name (:filename file)
;                       :mime (:content-type file)))
;                true) nil)]
;   (.delete (:tempfile file))
;   (if ret
;     (let [path (str "http://s3.amazonaws.com/PoliConnect/"
;                     ;(URLEncoder/encode (:filename file))
;                     )]
;       (layout "File caricato"
;         "Il file è stato caricato correttamente in "
;         (link-to path path)))
;     (layout "Errore"
;       "fail"))))

;(defpage "/testb" []
; (layout "hehe"
;   [:a {:href (str "mailto:s162270@studenti.polito.it?Subject=prova&Body="
;                   "ma che bello è??")}
;    "Email"]))

;(defpage "/test" []
;  {:status 303
;   :headers {"Content-Disposition" "attachment; filename=\"fname.jpg\""
;             "Location" "http://s3-eu-west-1.amazonaws.com/fede-test/cat.jpg"}})

;(defpage "/test3" []
;  {:status 303
;   :headers {"Content-Disposition" "attachment; filename=\"fname.jpg\""
;             "Location" "http://s3-eu-west-1.amazonaws.com/fede-test/cpu.svg"}})

;(defpage "/test4" []
;  {:status 303
;   :headers {"Location" "http://s3-eu-west-1.amazonaws.com/fede-test/un/test.jpg"}})

;(s3/with-s3 auth
;  (s3/put-file! (java.io.File. "/home/federico/cat.jpg")
;     "fede-test" :name "un/test.jpg"))

;(defpage "/test2" []
;  {:status 200
;   :headers {"Content-Length" "566461"}
;   :body (io/input-stream
;           (s3/with-s3 auth
;              (s3/get-object "eurecom per sito.pdf" "PoliConnect")))})

;(defpage "/test5" []
;  (layout "hehe"
;    (s3/test-post)))

;(spit "/home/federico/form.txt" (s3/test-post))

;(io/input-stream
;  (s3/with-s3 auth
;            (s3/get-object "eurecom per sito.pdf" "PoliConnect")))

;(s3/with-s3 auth
;  (s3/put-file! (java.io.File. "/home/federico/Archivio/cat.jpg")
;     "fede-test" :attachment true))

;(try
;  (s3/with-s3 auth
;    (s3/put-file! (java.io.File. "/home/federico/Archivio/cat.jpg")
;      "fede-test" :attachment true))
;  (catch Exception exc
;    nil))

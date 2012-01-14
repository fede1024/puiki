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
 (:require [clojure.contrib.string :as str]
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
   (link-to (channel-path c) (or name (:name c)))
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
           [:h3.section
            [:a.nodecor {:href "#" :onClick (js-toggle-anim "#channels_" (:year c) "_" (:_id f))}
              [:img.year {:src "/images/users.png"}] (cardinali (:year c)) " anno"]]
           [:div.section {:id (str "channels_" (:year c) "_" (:_id f)) :style "display: none"}
            [:p (link-to (channel-path c) "Canale dedicato a " (:name c))]
            (for [course (fetch :courses :where {:field (:name f) :year (:year c)}
                                :sort {:name 1})]
              (if-let [ch (fetch-one :channels :where {:code (:code course)})]
                [:p (link-to (channel-path ch) (:name ch))]))]])]
       "Nessun canale presente.")))

(defpage "/channel/list" []
  (let [last-channels (when (current-id)
                        (:last-channels (fetch-one :people :where {:_id (current-id)})))]
    (layout "Tutti i canali"
      [:div.like_button (like-button "/channel/list")]
      (when (not (empty? last-channels))
        (html
          [:h1.section "Preferiti:"]
          [:ul.years
           (for [c (sort-by :name (map #(fetch-one :channels :where {:_id %}) last-channels))]
             [:li.year
              [:h3.section
               [:a.nodecor {:href (channel-path c)}
                [:img.year {:src "/images/users.png"}] (:name c)]]])]
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

(defpage "/channel/:id" {:keys [id]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})
        follows (when (current-id) ;; TODO: serve?
                  (into #{} (:follows (fetch-one :people :where {:_id (current-id)}))))]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (store-preferred-channel ch)
        (update! :channels {:_id id} {:$inc {:views 1}})
        (layout (:name ch)
          (when (= (session/flash-get) :new-channel) 
            [:p "Nuovo canale creato."])
          (when (= (session/flash-get) :new-course) 
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
              (link-to (encode-url "/user/new-course?" {:field (:field ch) :year (:year ch)})
                       "Crea nuovo corso")))
          ;[:p (link-to "/user/following" "Canali seguiti")]
          [:br]
          (let [pages (fetch :posts :where {:channel id :type "normal" :removed {:$ne true}}
                             :sort {:title 1})
                questions (fetch :posts :where {:channel id :type "question" :removed {:$ne true}}
                                 :sort {:created-at -1} :limit 5)]
            (html
              [:h2.lastQuestions [:img.middle {:src "/images/question-big.png"}] " Ultime domande: "]
              (if (empty? questions)
                [:p "Non ci sono ancora domande in questo canale."]
                (html 
                  (post-links questions)
                  [:p.right (link-to (str (channel-path ch) "/questions") "Mostra tutto")]))
              [:h2.lastPages [:img.middle {:src "/images/wiki.png"}] " Pagine wiki:"]
              (if (empty? pages)
                [:p "Non ci sono ancora pagine wiki in questo canale."]
                [:div.section
                 (for [page pages]
                   [:p [:a {:href (post-path page)}
                        [:img.edit {:src "/images/page.png"}] [:b (:title page)]]])])))
          [:p [:img.edit {:src "/images/users.png" :alt "Vis" :title "Visualizzazzioni"}]
              "Canale visualizzato " (or (:views ch) 0) " volte."])))))
            
(defpage "/channel/:id/:show" {:keys [id show]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (layout (:name ch)
          [:h1.section (link-to (channel-path ch) (:name ch))]
          (channel-description ch)
          ;[:p (channel-info ch)]
          ;[:p "Canale creato il: " (format-timestamp (:created-at ch))]
          ;[:p (link-to "/user/following" "Canali seguiti")]
          [:br]
          (cond (= show "news")
            (html
             [:h2.lastPages [:img.middle {:src "/images/page-big.png"}] "Tutte le pagine:"]
             (post-links (fetch :posts :where {:channel id :type "normal"}
                                :sort {:created-at -1})))
            (= show "questions")
            (html [:h2.lastQuestions [:img.middle {:src "/images/question-big.png"}] "Tutte le domande: "]
             (post-links (fetch :posts :where {:channel id :type "question"}
                                :sort {:created-at -1})))))))))

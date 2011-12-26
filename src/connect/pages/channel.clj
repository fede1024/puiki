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
            [:a {:href "#" :onClick (js-toggle-anim "#channels_" (:year c) "_" (:_id f))}
              [:img.year {:src "/images/users.png"}] (cardinali (:year c)) " anno"]]
           [:div.section {:id (str "channels_" (:year c) "_" (:_id f)) :style "display: none"}
            [:p (link-to (channel-path c) "Canale dedicato a " (:name c))]
            (for [course (fetch :courses :where {:field (:name f) :year (:year c)}
                                :sort {:name 1})]
              (if-let [ch (fetch-one :channels :where {:code (:code course)})]
                [:p (link-to (channel-path ch) (:name ch))]))]])]
       "Nessun canale presente.")))

(defpage "/channel/list" []
  (let [follows (when (current-id)
                  (into #{} (:follows (fetch-one :people :where {:_id (current-id)}))))]
    (layout "Tutti i canali"
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
   [:h2.section "Modifica: "]
   (form-to [:get "/edit/new-post"]
     [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
     (submit-button {:class "postNew"} "Crea nuovo post"))]
  [:div.sideBarSection
   [:h2.section "Cerca: "]
   (form-to [:get "/search"]
     [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
     (text-field {:class :channelSearchText :placeholder "Testo ricerca"} :text)
     (submit-button {:class "channelSearch"} "Cerca"))])

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
             (link-to {:class :postTitle} (post-path post) (:title post))]
            [:td.postLinkDate (format-timestamp-relative (:created-at post))]]
           [:tr.postLinkSpace]))))])

;; TODO: solo i link agli indirizzi? Dividere la funzione in due?
(defpartial channel-description [ch]
  (if (= (:type ch) "course")
    [:p "Il corso si applica agli studenti di:"
     [:ul.channels
      (for [course (fetch :courses :where {:code (:code ch)})]
        (if-let [channel (fetch-one :channels :where {:type :field :field (:field course) :year (:year course)})]
          [:li.channel (link-to (channel-path channel) (:field course) " " (:year course) "°anno")]
          [:li.channel (:field course) " " (:year course) "°anno"]))]]
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

(defpage "/channel/:id" {:keys [id]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})
        follows (when (current-id)
                  (into #{} (:follows (fetch-one :people :where {:_id (current-id)}))))]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (layout (:name ch)
          (when (= (session/flash-get) :new-channel) 
            [:p "Nuovo canale creato."])
          (when (= (session/flash-get) :new-course) 
            [:p "Nuovo corso creato."])
          (when (current-id)
            [:span {:id id}
             (channel-follow-buttons ch (if (get follows id) 'remove 'add) :only-button true)])
          [:h1.section (link-to (channel-path ch) (:name ch))]
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
          (let [posts (fetch :posts :where {:channel id :type {:$ne "answer"}
                                            :removed {:$ne true}}
                        :sort {:created-at -1})
                news (filter #(= (:type %) "normal") posts)
                questions (filter #(= (:type %) "question") posts)]
            (html
              [:div.news
               [:h2.section "Ultime notizie:"]
               (post-links (take 5 news))
               [:p (link-to (str (channel-path ch) "/news") "Mostra tutte le notizie")]]
              [:div.questions
               [:h2.section "Ultime domande: "]
               (post-links (take 5 questions))
               [:p (link-to (str (channel-path ch) "/questions") "Mostra tutte le domande")]])))))))
            
(defpage "/channel/:id/:show" {:keys [id show]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (layout (:name ch)
          (if (= (session/flash-get) :new) 
            [:p "Nuovo canale creato."])
          [:h1.section (link-to (channel-path ch) (:name ch))]
          (channel-description ch)
          ;[:p (channel-info ch)]
          ;[:p "Canale creato il: " (format-timestamp (:created-at ch))]
          [:p (link-to "/user/following" "Canali seguiti")]
          [:br]
          (html
            (cond (= show "news")
              [:div.news
               [:h2.section "Tutte le notizie:"]
               (post-links (fetch :posts :where {:channel id :type "normal"}
                             :sort {:created-at -1}))]
              (= show "questions")
              [:div.questions
               [:h2.section "Tutte le domande: "]
               (post-links (fetch :posts :where {:channel id :type "question"}
                             :sort {:created-at -1}))])))))))

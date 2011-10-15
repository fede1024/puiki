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

(defpartial channel-follow-buttons [c action & {:keys [only-button]}]
  (if (= action 'add)
    [:button {:class "follow"
              :onClick (js-post "/channel/follow" (:_id c)
                         {:channel-id (str (:_id c)) :action "add" :only-button only-button})}
     "Segui"]
    [:button {:class "follow"
              :onClick (js-post "/channel/follow" (:_id c)
                         {:channel-id (str (:_id c)) :action "remove" :only-button only-button})}
     "Non seguire più"]))

(defpartial channel-data [c & {:keys [follows]}]
  (if (get follows (:_id c))
    [:img.channel {:src "/images/red-dot.png"}]
    [:img.channel {:src "/images/dot.png"}])
  (link-to (channel-path c) (:name c))
  (when (current-id)
    (channel-follow-buttons c
      (if (get follows (:_id c)) 'remove 'add)))
  [:p.channelInfo (channel-info c)]
  (when (= (:type c) "group")
    [:p.channelDescription (:description c)]))

(defpage "/channel/list" []
  (let [follows (when (current-id)
                  (into #{} (:follows (fetch-one :people :where {:_id (current-id)}))))]
    (layout "Tutti i canali"
      (link-to "/user/new-course" "Crea nuovo corso")
      [:h1.section "Elenco Indirizzi di studio:"]
      [:ul.fields
       (for [f (fetch :fields :sort {:name 1})]
         (html [:li.field [:h2.section (:name f) ":"]]
           (let [channels (fetch :channels :where {:field (:name f)})
                 years (sort-by :year > (filter #(= (:type %) "field") channels))
                 courses (sort-by :name (fetch :courses :where {:field (:name f)}))]
               (if (empty? years)
                 [:p "Nessun canale per " (:name f)]
                 [:ul.channels
                  (for [c years]  ;; Canali per l'indirizzo
                    [:li.channel {:id (:_id c)}
                     (channel-data c :follows follows)])
                  (for [course courses] ;; Corsi
                    (let [ch (fetch-one :channels :where {:_id (:channel course)})]
                      [:li.channel {:id (:_id ch)}
                       (channel-data ch :follows follows)]))]))))]
      [:h1.section "Elenco Gruppi:"]
      [:ul.channels
       (for [c (fetch :channels :where {:type "group"} :sort {:name 1})]
        [:li.channel {:id (:_id c)}
         (channel-data c :follows follows)])])))

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
        (channel-data c :follows follows)))))

(defpartial followers [channel]
  (let [limit 10
        flw (shuffle (fetch :people :where {:follows (:_id channel)}))
        count (count flw)]
    (html [:h2.section "Followers: ("
           (min limit count) (when (> count limit) (str " di " count)) ")"]
      (people-table (sort-by :lastname (sort-by :firstname (take limit flw)))
        :lastname (current-id))
      (when (> count limit)
        [:p "..."]))))

(defpartial add-post [channel]
  [:h2.section "Modifica: "]
  (form-to [:get "/edit/new-post"]
    [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
    (submit-button {:class "postNew"} "Crea nuovo post"))
  [:h2.section "Cerca: "]
  (form-to [:get "/search"]
    [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
    (text-field {:class :channelSearchText :placeholder "Testo ricerca"} :text)
    (submit-button {:class "channelSearch"} "Cerca")))

(defpartial post-link [post]
  (when (not (:removed post))
    (let [vote (or (:vtotal post) 0)]
      [:table.postLink
       [:tr.postLinkTitle
        [:td.postLinkTitle 
         (link-to {:class :postTitle} (post-path post)
           (post-images (:type post)) " " (:title post))]
        [:td.postLinkVote
         [:span.voteValue (str (when (> vote 0) "+") (when (= vote 0) " ") vote)]]]
       [:tr.postInfo
        (let [p (fetch-one :people :where {:_id (:author post)})]
          [:td.postAuthor "Postato da: " (user-description p)])
        [:td.postDate (format-timestamp (:created-at post))]]
       [:tr
        [:td 
         (when (= (:type post) "question")
           (str "Risposte: " (or (:answers post) 0) " "))
         "Commenti: " (or (:comments-num post) 0)]]])))

(defpage "/channel/:id/" {:keys [id]}
  (let [id (obj-id id)
        ch (fetch-one :channels :where {:_id id})]
    (if (not ch)
      (render "/not-found")
      (binding [*sidebar* (html (add-post ch)
                            (followers ch))]
        (layout (:name ch)
          (if (= (session/flash-get) :new) 
            [:p "Nuovo canale creato."])
          [:h1.channelName "Canale: " (:name ch)]
          [:p (:description ch)]
          [:p (channel-info ch)]
          [:p "Canale creato il: " (format-timestamp (:created-at ch))]
          [:p
           (when (current-id)
            [:span {:id id}
             (let [follows (into #{} (:follows (fetch-one :people :where {:_id (current-id)})))]
               (channel-follow-buttons ch
                 (if (get follows id) 'remove 'add) :only-button true))])
           " " (link-to "/channel/list" "Elenco canali") " "
           (link-to "/user/following" "Canali seguiti")]
          [:br]
          (let [posts (fetch :posts :where {:channel id :type {:$ne "answer"}}
                          :sort {:created-at -1})
                news (filter #(= (:type %) "normal") posts)
                questions (filter #(= (:type %) "question") posts)]
            (html
              [:div.news
               [:h2.section "Notizie: " (count news)]
               (map post-link news)]
              [:div.questions
               [:h2.section "Domande: " (count questions)]
               (map post-link questions)])))))))

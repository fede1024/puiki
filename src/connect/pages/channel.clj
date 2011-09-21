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
  (or (:followers ch) 0) " persone.")

(defpage "/channel/list" []
  (layout "Tutti i canali"
    ;(map channel-table (fetch :channels))
    [:h2.section "Elenco Indirizzi di studio:"]
    [:ul.fields
     (for [f (fetch :fields :sort {:name 1})]
       (html [:li.field (:name f) ":"]
         [:ul.fieldChannels
          (let [channels (fetch :channels :where {:field (:name f)}
                           :sort {:year -1})]
            (if (empty? channels)
              "Nessuno"
              (for [c channels]
                [:li.fieldChannel [:img {:src "/images/dot.png" :height 10}] " "
                 (link-to (channel-path c) (:name c))
                 [:span.channelInfo (channel-info c)]])))]))]
    [:h2.section "Elenco Gruppi:"]
    [:ul.groups
     (for [c (fetch :channels :where {:type "group"} :sort {:name 1})]
       [:li.groupChannel [:img {:src "/images/dot.png" :height 10}] " "
        (link-to (channel-path c) (:name c))
        [:span.channelInfo (channel-info c)] [:br]
        [:span.channelDescription (:description c)]])]))

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
  [:h2.userSidebarTitle "Modifica: "]
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
          [:h2.section "Canale: " (:name ch)]
          [:p (:description ch)]
          [:p (channel-info ch)]
          [:p "Canale creato il: " (format-timestamp (:created-at ch))]
          [:br]
          (map post-link
            (fetch :posts :where {:channel id :type {:$ne "answer"}}
              :sort {:created-at -1})))))))

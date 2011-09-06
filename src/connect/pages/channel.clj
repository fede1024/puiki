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
  {:public "Pubblico" :private "Privato"})

;(defn channel-info [ch]
;  (str "Tipo canale: " (channel-types (:type ch))
;    (when (= (:type ch) "group")
;      (str " (" (privacy-options (:privacy ch)) ")"))
;    " - Post: " (or (:posts ch) 0) " Followers: " (or (:followers ch) "0")))

(defn channel-info [ch]
  (str "(Post: " (or (:posts ch) 0) " Followers: " (or (:followers ch) 0) ")"))

(defpartial channel-table [ch]
  [:div.channel
   [:table.channel
    [:tr.channelTitle
     [:td.channelName {:colspan 2} (link-to (channel-path ch) (:name ch))]]
    [:tr.channelInfo
     [:td.channelInfo (channel-info ch)]
     [:td.channelDate (format-timestamp (:created-at ch))]]
    [:tr.channelDescription
     [:td.channelDescription {:colspan 2} (:description ch)]]]])

(defpage "/channel/list" []
  (layout "Tutti i canali"
    ;(map channel-table (fetch :channels))
    [:h2 "Elenco Indirizzi di studio:"]
    [:ul.fields
     (for [f (fetch :fields :sort {:name 1})]
       (html [:li.field (:name f) ":"]
         [:ul.fieldChannels
          (for [c (fetch :channels :where {:field (:name f)}
                    :sort {:year -1})]
            [:li.fieldChannel [:img {:src "/images/dot.png" :height 10}] " "
             (link-to (channel-path c) (:name c))
             [:span.channelInfo (channel-info c)]])]))]
    [:h2 "Elenco Gruppi:"]
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
    (html [:h2.peopleTableTitle "Followers: ("
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
  [:h2.userSidebarTitle "Cerca: "]
  (form-to [:get "/search"]
    [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
    (text-field {:class :channelSearchText :placeholder "Testo ricerca"} :text)
    (submit-button {:class "channelSearch"} "Cerca")))

(defpage "/channel/:id/" {:keys [id]}
  (let [id (obj-id id)
        channel (fetch-one :channels :where {:_id id})]
    (if (not channel)
      (render "/not-found")
      (binding [*sidebar* (html (add-post channel)
                            (followers channel))]
        (layout (:name channel)
          [:h2 "Canale:"]
          (channel-table channel)
          [:h2 "Post:"]
          (map post-table
            (fetch :posts :where {:channel id :type {:$ne "answer"}}
              :sort {:created-at -1})))))))

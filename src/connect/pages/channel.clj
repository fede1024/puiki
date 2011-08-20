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
  {:normal "Normale" :group "Gruppo"
   :field "Indirizzo di studio" :course "Corso"})

(def privacy-options
  {:public "Pubblico" :private "Privato"})

(defn channel-info [ch]
  (str "Tipo canale:" (channel-types (:type ch))
    (when (= (:type ch) :group)
      (str " (" (privacy-options (:privacy ch)) ")"))
    " - Post:" (:posts ch) " Followers:" (count (:followers ch))))

(defpartial channel-table [ch]
  [:table.channels
   [:tr.channelTitle
    [:td.channelName {:colspan 2} (link-to (channel-path ch) (:name ch))]]
   [:tr.channelInfo
    [:td.channelInfo (channel-info ch)]
    [:td.channelDate (format-timestamp (:created-at ch))]]
   [:tr.channelDescription
      [:td.channelDescription {:colspan 2} (:description ch)]]])

(defpage "/channel/list" []
  (layout "Tutti i canali"
    [:h2 "Elenco canali di PoliWeb:"]
    (map channel-table (fetch :channels))))

(= "4e4fb33744ae9f38cf8c0dfc" (str (:channel (fetch-one :posts))))

(fetch :posts :where {:channel (object-id "4e4fb33744ae9f38cf8c0dfc")})

(defpage "/channel/:id/" {:keys [id]}
  (let [id (obj-id id)
        channel (fetch-one :channels :where {:_id id})]
    (if (not channel)
      (render "/not-found")
      (layout (:name channel)
        [:h2 "Canale:"]
        (channel-table channel)
        [:h2 "Post:"]
        (map post-table
          (fetch :posts :where {:channel id})))))) ;;TODO: fix

(ns connect.pages.search
  (:use connect.pages.layout
        connect.pages.utils
        connect.search
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

(defmacro exec-time [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [ret# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]))

(defpage "/search" {:keys [text channel-id]}
  (let [channel (when channel-id
                  (fetch-one :channels :where {:_id (obj-id channel-id)}))]
    (layout "Cerca"
      [:h2.section "Cerca: "]
      (form-to {:accept-charset "utf-8" } [:get "/search"]
        [:input {:type :hidden :name "channel-id" :value (:_id channel)}]
        [:table.search
         [:tr.searchText
          [:td.searchText
           (text-field {:class :searchText :placeholder "Testo ricerca"} :text text)]
          [:td.searchActions (submit-button {:class "search"} "Cerca testo")]]
         [:tr.searchInfo
          [:td.searchChannel
           (if channel
             [:p "Cerca nel canale " (link-to (channel-path channel) (:name channel))]
             [:p "Cerca in tutti i canali"])]]])
      (when (not (clojure.string/blank? text))
        (let [[posts exec-time] (exec-time (search-by-text text (:_id channel)))]
          (html
            [:h2.section "Risultati: " (count posts)]
            [:p "Tempo di elaborazione: " (int exec-time) " millisecondi"]
            (when (empty? posts)
              [:p "Nessun risultato"])
            [:table
             (for [post posts]
               (let [channel (fetch-one :channels :where {:_id (:channel post)})]
                 [:tr [:td (link-to (post-path post) (:title post))]
                  [:td (:name channel)]]))]))))))

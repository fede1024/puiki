(ns connect.pages.home
  (:use connect.pages.layout
        connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [connect.pages.channel :as channel]
           [noir.validation :as vali]
           [noir.response :as resp]
   [noir.util.test :as test]))

(defpage "/" []
  (layout "Poli Connect"
    [:h1.section "Poli Connect"]
    [:div
     [:h2.section "Link:"]
     [:p (link-to "/user/" "Pagina utenti")]
     [:p (link-to "/search" "Cerca")]
     [:p (link-to "/admin/" "Pagina amministratore")]
     [:p (link-to "/channel/list" "Elenco canali")]]
    [:h2.section "Post più recenti:"]
    (let [posts (fetch :posts :where {:removed {:$ne true}}
                  :sort {:created-at -1} :limit 10)]
      (channel/post-links posts))))

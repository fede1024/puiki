(ns connect.pages.home
  (:use connect.pages.layout
        connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]
   [noir.util.test :as test]))

(defpage "/" []
  (layout "Poli Connect"
    [:div
     [:h2.section "Home"]
     [:p (link-to "/user/" "Pagina utenti")]
     [:p (link-to "/search" "Cerca")]
     [:p (link-to "/admin/" "Pagina amministratore")]
     [:p (link-to "/channel/list" "Elenco canali")]]))

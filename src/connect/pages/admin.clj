(ns connect.pages.admin
  (:use connect.pages.layout
        connect.pages.utils
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

(pre-route "/admin*" {}
  (when-not (admin? (current-id))
    (resp/redirect "/login")))

(defpage "/admin/" {}
  (layout "PoliWeb"
    [:h2 "Amministrazione:"]
    [:p (link-to "/admin/fields" "Modifica indirizzi di studio")]
    [:h2 "Altro:"]
    [:p (link-to "/admin/zero" "/admin/zero")
     " Effettua una divisione per zero. "
     "Verifica il funzionamento della cattura degli errori"]
    [:p (link-to "/logs/errors/" "/logs/errors/")
     " Log degli errori."]
    [:p (link-to "/admin/recur3" "/admin/recur3") 
     " Mostra ricorsivamente il layout."]))

(defpage "/admin/zero"[]
  (layout "Zero"
    [:h2 "Divido per zero: " (/ 1 0)]))

(defpage "/admin/fields" [& [field]]
  (layout "Amministrazione"
    (vali/on-error :name error-text)
    [:h2 "Aggiungi indirizzo di studio:"]
    (form-to {:accept-charset "utf-8"} [:post "/admin/add-field"]
      (text-field {:size 25} :name
        (if (vali/errors? :name) (or (:name field) "")))
      (submit-button "Aggiungi"))
    [:h2 "Cancella indirizzo di studio:"]
    [:table
     (for [field (fetch :fields)]
       [:tr [:td (:name field)]
        [:td (form-to [:post "/admin/rem-field"]
               (hidden-field :name (:name field))
               (submit-button "Rimuovi"))]])]))

(defn valid? [{:keys [name]}]
  (vali/rule (vali/has-value? name)
     [:name "Il campo non deve essere vuoto."])
  (vali/rule (not (fetch-one :fields :where {:name name}))
     [:name "Esiste già."])
  (not (vali/errors? :name)))

(defpage [:post "/admin/add-field"] {:as field}
  (println (pr-str field))
  (if (valid? field)
    (do
      (insert! :fields field)
      (resp/redirect "/admin/fields"))
    (render "/admin/fields" field)))

(defpage [:post "/admin/rem-field"] {:as field}
  (destroy! :fields {:name (:name field)})
  (resp/redirect "/admin/fields"))

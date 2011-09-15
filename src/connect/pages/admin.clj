(ns connect.pages.admin
  (:use connect.pages.layout
        connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require connect.errors
           [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(pre-route "/admin*" p
  (if (current-id)
    (if (not (admin? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (:uri p)})))

(defpage "/admin/" {}
  (layout "PoliWeb"
    [:h2.section "Amministrazione:"]
    [:p (link-to "/admin/fields" "Modifica indirizzi di studio")]
    [:h2.section "Altro:"]
    [:p (link-to "/admin/zero" "/admin/zero")
     " Effettua una divisione per zero. "
     "Verifica il funzionamento della cattura degli errori"]
    [:p (link-to "/logs/errors/" "/logs/errors/")
     " Log degli errori."]
    [:p (link-to "/admin/recur3" "/admin/recur3") 
     " Mostra ricorsivamente il layout."]))

(defpage "/admin/zero"[]
  (layout "Zero"
    [:h2.section "Divido per zero: " (/ 1 0)]))

(defpage "/admin/fields" [& [field]]
  (layout "Amministrazione"
    (vali/on-error :name error-text)
    [:h2.section "Aggiungi indirizzo di studio:"]
    (form-to {:accept-charset "utf-8"} [:post "/admin/add-field"]
      (text-field {:size 25} :name
        (if (vali/errors? :name) (or (:name field) "")))
      (submit-button "Aggiungi"))
    [:h2.section "Cancella indirizzo di studio:"]
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
  (if (valid? field)
    (do
      (insert! :fields (merge field {:created-at (java.util.Date.)}))
      (resp/redirect "/admin/fields"))
    (render "/admin/fields" field)))

(defpage [:post "/admin/rem-field"] {:as field}
  (destroy! :fields {:name (:name field)})
  (resp/redirect "/admin/fields"))

(pre-route "/logs*" p
  (if (current-id)
    (if (not (admin? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (:uri p)})))

(defpage "/logs/errors/" []
  (layout "Log Errori"
    (let [files (sort-by #(.getName %)
                  (.listFiles (java.io.File. connect.errors/*errors-dir*)))]
      [:span 
       [:h2.section "Log errori:"]
       [:p "Numero massimo: " connect.errors/*max-error-files*]
       [:p "Directory: " connect.errors/*errors-dir*]
       (for [f (reverse files)]
         [:p (link-to (str "./" (.getName f)) (.getName f))])
       (form-to [:get "/logs/errors/delete"]
         (submit-button "Cancella tutti"))])))

(defpage "/logs/errors/delete" []
  (do (connect.errors/remove-all-errors)
    (resp/redirect "/logs/errors/")))
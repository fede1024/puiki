(ns connect.pages.admin
  (:use connect.logs
        connect.pages.layout
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

(pre-route "/admin*" request
  (if (current-id)
    (if (not (admin? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (get-request-uri request)})))

(defpage "/admin/" {}
  (layout "PoliConnect"
    [:h2.section "Amministrazione:"]
    [:p (link-to "/admin/fields" "Modifica indirizzi di studio")]
    [:h2.section "Altro:"]
    [:p (link-to "/admin/zero" "/admin/zero")
     " Effettua una divisione per zero. "
     "Verifica il funzionamento della cattura degli errori"]
    [:p (link-to "/logs/errors/" "/logs/errors/")
     " Log degli errori."]
    [:p (link-to "/admin/logs" "/admin/logs")
     " Log deglle pagine."]
    [:p (link-to "/admin/recur3" "/admin/recur3") 
     " Mostra ricorsivamente il layout."]))

(defpage "/admin/zero" []
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

(pre-route "/logs*" request
  (if (current-id)
    (if (not (admin? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (get-request-uri request)})))

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

(defpage "/admin/recur:n" {:keys [n]}
  (reduce #(layout %2 %1) [:p "Contenuto pagina"]
    (range (Integer/parseInt n))))

(defpage "/admin/logs" {:keys [session n] :or {n "500"}}
  (let [logs (log-tail (Integer/parseInt n) :session session)
        grouped (sort-by first (group-by :session logs))]
    (layout "Logs"
      (for [[session s-logs] grouped]
        [:table.logs
         [:caption "Session " (if session 
                                (link-to (str "/admin/logs?session=" session "&n=" n) session)
                                "new")]
         [:tr.logs [:th "Date"] [:th.logs "Time"] [:th.logs "Method"]
          [:th.logs "URL"] [:th.logs "Type"] [:th.logs "ID"]]
         (for [log s-logs]
           [:tr.logs
            [:td.logs (format-log-date (:date log))]
            [:td.logsB (:resp-time log)]
            [:td.logsB (:method log)]
            [:td.logsB (:uri log)]
            [:td.logsB (:out-type log)]
            [:td.logsB (:username log)]])]))))


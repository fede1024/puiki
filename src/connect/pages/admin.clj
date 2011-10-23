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
    [:p (link-to "/admin/feedbacks" "/admin/feedbacks")
     " Feedback degli utenti."]
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

(defpartial session-logs-table [n session-str]
  (let [session (if (str/blank? session-str) nil session-str)
        logs (log-tail-by-session n session)]
    [:table.logs
     [:caption "Session " (or session "NEW")]
     [:tr.logs [:th "IP"] [:th "Date"] [:th.logs "Time"] [:th.logs "Method"]
      [:th.logs "URL"] [:th.logs "Status"] [:th.logs "Type"] [:th.logs "ID"]]
     (for [log logs]
       [:tr.logs
        [:td.logs (:ip log)]
        [:td.logsB (format-log-date (:date log))]
        [:td.logsB (:resp-time log)]
        [:td.logsB (:method log)]
        [:td.logsB (:uri log)]
        [:td.logsB (:status log)]
        [:td.logsB (:out-type log)]
        [:td.logsB (:username log)]])]))

(defpartial logs-table [n]
  (let [logs (log-tail n)]
    [:table.logs
     [:caption "All sessions"]
     [:tr.logs [:th "Session"] [:th.logs "IP"] [:th.logs "Date"] [:th.logs "Time"] [:th.logs "Method"]
      [:th.logs "URL"] [:th.logs "Status"] [:th.logs "Type"] [:th.logs "ID"]]
     (for [log logs]
       [:tr.logs
        [:td.logs (if (:session log)
                    (link-to (str "/admin/logs?session=" (:session log) "&n=" n)
                      (subs (:session log) 0 5))
                    (link-to (str "/admin/logs?session=&n=" n) "NEW"))]
        [:td.logsB (:ip log)]
        [:td.logsB (format-log-date (:date log))]
        [:td.logsB (:resp-time log)]
        [:td.logsB (:method log)]
        [:td.logsB (:uri log)]
        [:td.logsB (:status log)]
        [:td.logsB (:out-type log)]
        [:td.logsB (:username log)]])]))

(defpage "/admin/logs" {:keys [session n] :or {n "50"}}
  (let [num (Integer/parseInt n)]
    (if session
      (layout "Logs"
        (session-logs-table num session))
      (layout "Logs"
;        [:table.logs
;         [:caption "Active sessions"]
;         [:tr.logs [:th "Identifier"] [:th.logs "data"]]
;         (for [[id data] @noir.session/mem]
;           [:tr.logs
;            [:td.logs (link-to (str "/admin/logs?session=" id "&n=" n) id)]
;            [:td.logsB (pr-str data)]])]
        (logs-table num)))))

(defpage "/admin/feedbacks" {:keys [n] :or {n "20"}}
  (let [feeds (if (= n "all")
                (fetch :feedbacks)
                (fetch :feedbacks :limit (Integer/parseInt n)))]
    (layout ""
      [:h1.section "Feedbacks: "]
      [:h2.section "Visualizzati " (count feeds) ", limite: " n]
      (for [feed feeds]
        (let [person (fetch-one :people :where {:_id (:person feed)})]
          [:p (:firstname person) " " (:lastname person) " (" (:_id person) ") scrive:"
           [:br] (:text feed) [:br] (format-timestamp-relative (:created-at feed))]))
      [:p "Mostra: "
       [:a {:href "/admin/feedbacks?n=100"} "100"] " "
       [:a {:href "/admin/feedbacks?n=all"} "tutti"]])))

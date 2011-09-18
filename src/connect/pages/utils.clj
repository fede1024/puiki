(ns connect.pages.utils
  (:use hiccup.core
    somnium.congomongo)
  (:require [noir.session :as session]))

(defn current-id []
  (session/get :username))

(defn translate-job [job]
  (cond (= job "student") "Studente"
        (= job "professor") "Docente"
        (= job "admin") "Amministratore"
    :else "Ruolo non conosciuto"))

(defn user-edit-path [id]
  (str "/user/" id "/edit"))

(defn user-info-path [id]
  (str "/user/" id "/info"))

(defn channel-path [channel]
  (str "/channel/" (:_id channel) "/"))

(defn user-reply-path [post]
  (str "/edit/reply/" (:_id post)))

(defn user-comment-path [post]
  (str "/edit/comment/" (:_id post)))

(defn post-path [post]
  (str "/post/" (:_id post)))

(defn post-vote-path [post]
  (str "/edit/vote/" (:_id post)))

(defn post-remove-path [post]
  (str "/edit/remove/" (:_id post)))

(defn user? [id] ;; TODO: Migliorare qui
  (when id
    (some #{"user"}
      (:roles (fetch-one :people :where {:_id id})))))

(defn admin? [id] ;; TODO: Migliorare qui
  (when id
    (some #{"admin"}
      (:roles (fetch-one :people :where {:_id id})))))

(defn format-timestamp [ts]
  (if ts
    (.format (java.text.SimpleDateFormat. "d-M-yy HH:mm") ts)
    "???"))

(defn get-request-uri [req]
  (str (:uri req) "?" (:query-string req)))

;(defn current-url-hidden []
;  [:script {:type "text/javascript"}
;             "document.write(\"<input type='hidden' name='url' value='\" + document.URL + \"'>\");"])

(defn html-redirect [url & {:keys [wait] :or {wait 0}}]
  (html [:meta {:http-equiv :REFRESH
                :content (str wait ";url=" url)}]))

(defn js-redirect [url]
  (html
    [:html
     [:head
      [:script {:type "text/javascript" :src "/jquery-1.6.4.min.js"}]
      [:script {:type "text/javascript"}
       "$(document).ready(function() {"
       (str "$('body').load('" url "');")
       "});"]]
     [:body ]]))

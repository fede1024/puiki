(ns connect.pages.utils
  (:use somnium.congomongo)
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
  (str "/user/reply/" (:_id post)))

(defn user-comment-path [post]
  (str "/user/comment/" (:_id post)))

(defn post-path [post]
  (str "/post/" (:id post)))

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

(ns connect.pages.utils
  (:use hiccup.core
    somnium.congomongo)
  (:require [clojure.contrib.string :as str]
    [noir.session :as session])
  (:import [java.util Locale Date Calendar GregorianCalendar]
    [java.text SimpleDateFormat DateFormatSymbols]))

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
  (str "/channel/" (:_id channel)))

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
    (.format (SimpleDateFormat. "dd-MM-yyyy HH:mm") ts)
    "???"))

;(def dates (map :created-at (fetch :posts)))

(def month-names 
  (.getMonths (DateFormatSymbols. (Locale/ITALIAN))))

(def days-names 
  (.getWeekdays (DateFormatSymbols. (Locale/ITALIAN))))

(defn format-timestamp-relative [date]
  (let [diff (int (/ (- (.getTime (Date.))
                       (.getTime date))
                    1000))]
    (cond (< diff 60)
      "Pochi secondi fa"
      (< diff 3600)
      (let [n (int (/ diff 60))]
        (if (= n 1)
          "Un minuto fa"
          (str n " minuti fa")))
      (< diff 86400) ;; Giorno
      (let [n (int (/ diff 3660))]
        (if (= n 1)
          "Un ora fa"
          (str n " ore fa")))
      (< diff 604800) ;; Settimana
      (let [cal (Calendar/getInstance)]
        (.setTime cal date)
        (nth days-names (.get cal Calendar/DAY_OF_WEEK)))
      :else
      (let [cal (Calendar/getInstance)]
        (.setTime cal date)
        (str (.get cal Calendar/DAY_OF_MONTH)
          " " (nth month-names (.get cal Calendar/MONTH))
          " " (.get cal Calendar/YEAR))))))

;(dorun
;  (map (fn [date] (println (format-timestamp date) " - " (format-timestamp-relative date)))
;    (sort dates)))

(defn get-request-uri [req]
  (if (str/blank? (:query-string req))
    (:uri req)
    (str (:uri req) "?" (:query-string req))))

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

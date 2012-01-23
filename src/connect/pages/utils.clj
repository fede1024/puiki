(ns connect.pages.utils
  (:use noir.core
        hiccup.core
        hiccup.page-helpers
    somnium.congomongo)
  (:require [clj-json.core :as json]
            [clojure.contrib.string :as str]
            [noir.session :as session])
  (:import [java.util Locale Date Calendar GregorianCalendar]
    [java.text SimpleDateFormat DateFormatSymbols]
    [java.net URLEncoder URLDecoder]))

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
  (if (= (:type post) "answer")
    (str "/post/" (:answers-to post) "#post" (:_id post))
    (str "/post/" (:_id post))))

(defn edit-path [post]
  (str "/edit/post/" (:_id post)))

(defn old-pages-path [page]
  (str "/old-pages/" (:_id page)))

(defn post-vote-path [post]
  (str "/edit/vote/" (:_id post)))

(defn post-remove-path [post]
  (str "/edit/remove/" (:_id post)))

(defn file-path [channel file &{:keys [action]}]
  (str "/channel/" channel "/files/" (URLEncoder/encode file)
       (when action (str "?action=" action))))

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
  
(defn timestamp-relative-to-string [date]
  (let [diff (int (/ (- (.getTime (Date.))
                       (.getTime date))
                    1000))]
    (cond (< diff 60)
      "pochi secondi fa"
      (< diff 3600)
      (let [n (int (/ diff 60))]
        (if (= n 1)
          "un minuto fa"
          (str n " minuti fa")))
      (< diff 86400) ;; Giorno
      (let [n (int (/ diff 3600))]
        (if (= n 1)
          "un ora fa"
          (str n " ore fa")))
      (< diff 518400) ;; sei giorni
      (let [cal (Calendar/getInstance)]
        (.setTime cal date)
        (nth days-names (.get cal Calendar/DAY_OF_WEEK)))
      :else
      (let [cal (Calendar/getInstance)]
        (.setTime cal date)
        (str (.get cal Calendar/DAY_OF_MONTH)
          " " (nth month-names (.get cal Calendar/MONTH))
          " " (.get cal Calendar/YEAR))))))

(defn format-timestamp-relative [date]
  (if date
    (timestamp-relative-to-string date)
    "???"))

(defn to-integer [value & [alt]]
  (if value 
    (if (integer? value)
      value
      (try (Integer/parseInt value)
        (catch Exception e (or alt 0))))
    (or alt 0)))

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
  (str "$(document).ready(function() {"
       "$('body').load('" url "');"
       "});"))

(defn js-redirect-page [url]
  (html
    [:html
     [:head
      [:script {:type "text/javascript" :src "/jquery-1.7.1.min.js"}]
      [:script {:type "text/javascript"}
       "$(document).ready(function() {"
       (str "$('body').load('" url "');")
       "});"]]
     [:body ]]))

(defn encode-url [url params]
  (str url "?" (encode-params params)))

(defn js-do [& objs]
  (apply str objs))

(defn js-show [& objs]
  (str "$('" (apply str objs) "').css('display', '');"))

(defn js-show-anim [& objs]
  (str "$('" (apply str objs) "').show('clip');"))

(defn js-toggle-anim [& objs]
  (str "$('" (apply str objs) "').toggle('medium');"))

(defn js-hide [& objs]
  (str "$('" (apply str objs) "').css('display', 'none');"))

(defn js-get [path id opts]
  (str "$('#loader').css('display', 'inline');"
    "$.get('" path "', " (json/generate-string opts) ", function(content) {$('#" id "').html(content);"
    "$('#loader').css('display', 'none');});"))

(defn js-post [path id opts]
  (str "$('#loader').css('display', 'inline');"
    "$.post('" path "', " (json/generate-string opts) ", function(content) {$('#" id "').html(content);"
    "$('#loader').css('display', 'none');});"))

(defpartial jquery-accordion [id]
  [:script {:type "text/javascript"}
   "$(function() {$( '#" id "' ).accordion();});"])

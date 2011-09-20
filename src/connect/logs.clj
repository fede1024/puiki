(ns connect.logs
  (:use connect.utils
    somnium.congomongo)
  (:import [java.util Calendar GregorianCalendar])
  (:require [noir.session :as session]))

;db.createCollection("mycoll", {capped:true, size:10000000})

(defn wrap-logging [handler]
  (fn [req]
    (let [[out exec-time] (exec-time (handler req))]
      (insert! :logs
        {:date (java.util.Date.) :resp-time exec-time
         :method (:request-method req) :uri (:uri req)
         :referer (get-in req [:headers "referer"])
         :session (:value (get-in req [:cookies "ring-session"]))
         :status (:status out) :out-type (get-in out [:headers "Content-Type"])
         :username (session/get :username)})
      out)))

(defn format-log-date [date]
  (.format (java.text.SimpleDateFormat. "yy-MM-dd HH:mm:ss") date))

(defn print-log [log]
  (println
    (format "%s (%4.1f) %6s  %4s  %s  %s" (format-log-date (:date log))
      (or (:resp-time log) 0) (or (:username log) "nil")
      (:method log) (:uri log) (:out-type log))))

(defn get-time-ago [& {:keys [years months days hours minutes seconds]}]
  (let [d (GregorianCalendar.)]
    (.add d Calendar/YEAR   (- (or years 0)))
    (.add d Calendar/MONTH  (- (or months 0)))
    (.add d Calendar/DAY_OF_MONTH (- (or days 0)))
    (.add d Calendar/HOUR   (- (or hours 0)))
    (.add d Calendar/MINUTE (- (or minutes 0)))
    (.add d Calendar/SECOND (- (or seconds 0)))
    (.getTime d)))

(defn logs-since [& keys]
  (dorun (map print-log
           (fetch :logs :where {:date {:$gt (apply get-time-ago keys)}}))))

(defn delete-logs! []
  (destroy! :logs {}))

(defn print-logs [& [num]]
  (let [tail (reverse (fetch :logs :sort {:$natural -1} :limit (or num 100)))]
    (dorun (map print-log tail))))

;(print-logs 200)
;(print-log (logs-since :hours 3))

(ns connect.logs
  (:use connect.utils
    somnium.congomongo)
  (:import [java.util Calendar GregorianCalendar])
  (:require [noir.session :as session]))

(defn wrap-logging [handler]
  (fn [req]
    (let [[out exec-time] (exec-time (handler req))
          headers (:headers req)
          cookies (:cookies req)]
      (insert! :logs
        {:date (java.util.Date.) :resp-time exec-time
         :method (:request-method req) :uri (:uri req)
         :referer (get headers "referer")
         :session (:value (get cookies "ring-session"))
         :status (:status out) :out-type (get headers "Content-Type")
         :username (session/get :username)})
      out)))

(defn format-log-date [date]
  (.format (java.text.SimpleDateFormat. "yy-MM-dd HH:mm:ss") date))

(GregorianCalendar. 2008 Calendar/APRIL 16)

(defn print-log [logs]
  (doseq [log logs]
    (println
      (format "%s (%4.1f) %6s  %4s  %s" (format-log-date (:date log))
        (or (:resp-time log) 0) (or (:username log) "nil")
        (:method log) (:uri log)))))

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
  (fetch :logs :where {:date {:$gt (apply get-time-ago keys)}}))

(defn delete-logs! []
  (destroy! :logs {}))

;(print-log (logs-since :hours 3))

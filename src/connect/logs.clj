(ns connect.logs
  (:use connect.utils
    somnium.congomongo)
  (:import [java.util Calendar GregorianCalendar])
  (:require [noir.session :as session]))

;db.createCollection("mycoll", {capped:true, size:10000000})

(def ^:dynamic *ip* nil)
(def ^:dynamic *user-agent* nil)

(defn wrap-logging [handler]
  (fn [req]
    (binding [*ip* (:remote-addr req)
              *user-agent* (get-in req [:headers "user-agent"])]
      (let [[out exec-time] (exec-time (handler req))]
        (insert! :logs
          {:date (java.util.Date.) :resp-time exec-time
           :method (:request-method req) :uri (:uri req)
           :referer (get-in req [:headers "referer"])
           :user-agent (get-in req [:headers "user-agent"])
           :session (:value (get-in req [:cookies "ring-session"]))
           :status (:status out) :out-type (get-in out [:headers "Content-Type"])
           :username (session/get :username) :ip (:remote-addr req)})
        out))))

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
  (fetch :logs :where {:date {:$gt (apply get-time-ago keys)}}))

(defn print-logs-since [& keys]
  (dorun (map print-log (apply logs-since keys))))

(defn delete-logs! []
  (destroy! :logs {}))

(defn all-logs []
  (fetch :logs
    :where {:out-type {:$nin ["image/x-icon" "image/png" "image/gif" "text/css" "text/javascript"]}}))

(defn log-tail [num]
  (reverse
    (fetch :logs
      :where {:out-type {:$nin ["image/x-icon" "image/png" "image/gif" "text/css" "text/javascript"]}}
      :sort {:$natural -1} :limit num)))

(defn log-tail-by-session [num session]
  (reverse
    (fetch :logs
      :where {:out-type {:$nin ["image/x-icon" "image/png" "image/gif" "text/css" "text/javascript"]}
              :session session}
      :sort {:$natural -1} :limit num)))

(defn print-logs [num]
  (dorun (map print-log (log-tail num))))

(defn analize-logs-resp-time [& [n]]
  (let [all-logs (log-tail (or n 10000))
        sum (reduce + (map :resp-time all-logs))
        n (count all-logs)]
    (println "Elaborati" n "logs, a partire dal" (format-log-date (:date (first all-logs))))
    (println "Tempo di risposta medio:" (int (/ sum n)) "millisecondi")))

;(log-tail 10 :session "ac51d74a-d0f9-495b-80e2-4547fd5f4ed6")
;(fetch :logs :where {:uri "/logout"})
;(print-logs-since :hours 1)
;(logs-since :hours 1)
;
;(group-by :session (log-tail 100))
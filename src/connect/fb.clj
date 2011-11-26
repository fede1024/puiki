(ns connect.fb
  (:use somnium.congomongo)
  (:require [clj-json.core :as json]
            [clj-http.client :as client]
            [clojure.contrib.string :as str])
  (:import [org.apache.commons.codec.binary Base64]))

; (def str-enc "lmvcn3laaEjM8_InLIYZGeJo32PMFf7mgdiaWOF8lXM.eyJhbGdvcml0aG0iOiJITUFDLVNIQTI1NiIsImV4cGlyZXMiOjEzMjAzMzYwMDAsImlzc3VlZF9hdCI6MTMyMDMzMDc5NSwib2F1dGhfdG9rZW4iOiJBQUFEU3F6cGFQbjBCQU1QTkZLSnpMWHR6OVpBSkQ5T09KMGp5TnBIcWtYY21hS0JWY05Wc3prU00xYnBSTHNTNGFaQnFHR1ZLRzhtNXgzb1Vlc2ZYWkJWYms4N3FtZVVUdEJnYmhqTzhjUWN2TzB6UGNnaCIsInVzZXIiOnsiY291bnRyeSI6Iml0IiwibG9jYWxlIjoiaXRfSVQiLCJhZ2UiOnsibWluIjoyMX19LCJ1c2VyX2lkIjoiMTQ3NTExODgwOSJ9")

(def app-id "231632860233341")
(def app-secret "9e19bb3cc6680e4b530284f0d6679d04")

(defn decode-string-request [str]
  (let [payload (-> (second (.split str "[.]" 2))
                    (.replace "-" "+")
                    (.replace "_" "/")
                    .trim)
        json-str (-> payload
                     .getBytes
                     Base64/decodeBase64
                     String.)]
    (json/parse-string json-str)))

(defn get-access-token [app secret]
  (let [resp (client/post "https://graph.facebook.com/oauth/access_token"
                          {:query-params {:client_id app :client_secret secret
                                          :grant_type "client_credentials"}
                           :throw-exceptions false})]
    (if (= (:status resp) 200)
      (second (str/split #"=" (:body resp)))
      (println "FB Access Token:" (get-in (json/parse-string (:body resp)) ["error" "message"])))))

;(def acc (get-access-token app-id app-secret))

;(let [resp (client/post (str "https://graph.facebook.com/" "fgtech" "/apprequests")
;                          {:query-params {:message "Ciao :)"                                          :access_token acc}
;                           :throw-exceptions false})]
;  (if (= (:status resp) 200)
;      (:body resp)
;      (println "FB App Request:" (get-in (json/parse-string (:body resp)) ["error" "message"]))))

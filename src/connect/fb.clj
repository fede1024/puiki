(ns connect.fb
  (:use somnium.congomongo)
  (:require [clj-json.core :as json]
            [clj-http.client :as client])
  (:import [org.apache.commons.codec.binary Base64]))

; (def str-enc "lmvcn3laaEjM8_InLIYZGeJo32PMFf7mgdiaWOF8lXM.eyJhbGdvcml0aG0iOiJITUFDLVNIQTI1NiIsImV4cGlyZXMiOjEzMjAzMzYwMDAsImlzc3VlZF9hdCI6MTMyMDMzMDc5NSwib2F1dGhfdG9rZW4iOiJBQUFEU3F6cGFQbjBCQU1QTkZLSnpMWHR6OVpBSkQ5T09KMGp5TnBIcWtYY21hS0JWY05Wc3prU00xYnBSTHNTNGFaQnFHR1ZLRzhtNXgzb1Vlc2ZYWkJWYms4N3FtZVVUdEJnYmhqTzhjUWN2TzB6UGNnaCIsInVzZXIiOnsiY291bnRyeSI6Iml0IiwibG9jYWxlIjoiaXRfSVQiLCJhZ2UiOnsibWluIjoyMX19LCJ1c2VyX2lkIjoiMTQ3NTExODgwOSJ9")

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

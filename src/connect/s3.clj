(ns connect.s3
  "Basic S3 utils"
  (:refer-clojure :exclude [list])
  (:use somnium.congomongo)
  (:import
    (org.jets3t.service.security AWSCredentials ProviderCredentials)
    (org.jets3t.service.acl AccessControlList)
    (org.jets3t.service S3Service)
    (org.jets3t.service.impl.rest.httpclient RestS3Service)
    (org.jets3t.service.model S3Object)
    (javax.activation MimetypesFileTypeMap)
    (java.util Calendar GregorianCalendar))
  (:require [noir.util.s3 :as s3]))

(def ^:dynamic *s3* nil)

(defn credentials 
  "Create an S3 service object"
  [{secret :secret-key access :access-key}]
  (new AWSCredentials access secret))

(defn service 
  "Create an S3 service object"
  [{secret :secret-key access :access-key}]
  (new RestS3Service (new AWSCredentials access secret)))

;(def inputs [;"<input type=\"hidden\" name=\"acl\" value=\"public-read\">"
;             ;"<input type=\"hidden\" name=\"Content-Type\" value=\"image/jpeg\">"
;             ;"<input type=\"hidden\" name=\"success_action_redirect\" value=\"http://localhost/post_upload\">"
;             ])

;(def conditions [(S3Service/generatePostPolicyCondition_Equality "bucket" "test-bucket")
;                 (S3Service/generatePostPolicyCondition_Equality "key" "file.jpg")
;                 (S3Service/generatePostPolicyCondition_Range 10240 204800)])

;(defn test-post []
;  (S3Service/buildPostForm "test-bucket" "file.jpg" (credentials auth) (get-time-add 30)
;                           (into-array conditions), (into-array String inputs), nil, true))

(defn put-file! [file bucket &{:keys [name mime attachment public]}]
  (let [obj (new S3Object file)
        obj-mime (or mime
                     (. (new MimetypesFileTypeMap)
                       getContentType file))
        obj-name (or name (. file getName))]
    (when public
      (.setAcl obj AccessControlList/REST_CANNED_PUBLIC_READ));
    (. obj setContentType obj-mime)
    (. obj setName obj-name)
    (when attachment
      (. obj addMetadata "Content-Disposition"
        (str "attachment; filename=\"" obj-name "\";")));
    (. *s3* putObject bucket obj)))

(defn list-bucket [bucket & [not-recursive]]
  (let [delimiter (when not-recursive "/")]
    (for [obj (. *s3* listObjects (. *s3* getBucket bucket) "" delimiter)]
      (. obj getName))))

(defn get-object [object bucket]
  (. *s3* getObject (. *s3* getBucket bucket) object))

;(with-s3 auth
;  (let [obj (new S3Object "cat.jpg")]
;    (. *s3* copyObject "files.policonnect.it" "cat.jpg"
;      "files.puiki.it" obj false)))

;(with-s3 auth
;  (let [obj (get-object "cat.jpg" "test.policonnect.it")]
;    (.getAcl obj)))

;(with-s3 auth
;  (let [obj (get-object "4ed2a94ee4b0b5637ab5bc14/fortune.sh" "test.policonnect.it")]
;    (.getAcl obj)))

;(with-s3 auth
;  (let [acl (AccessControlList.)
;        obj (get-object "4ed2a94ee4b0b5637ab5bc14/fortune.sh" "test.policonnect.it")]
;    (.grantPermission acl GroupGrantee/ALL_USERS Permission/PERMISSION_READ)
;    (.setAcl obj acl)))

(defn get-object-stream [object bucket]
  (. (. *s3* getObject (. *s3* getBucket bucket) object)
    getDataInputStream))

(defmacro with-s3 [server-spec & body]
  `(binding [*s3* (service ~server-spec)]
     ~@body))

(defn get-time-add [minutes]
  (let [c (Calendar/getInstance)]
    (.add c Calendar/MINUTE minutes)
    (.getTime c)))

(defn get-expiring-url [name bucket minutes & {:keys [virtual ssl]}]
  (let [url (. *s3* createSignedGetUrl bucket name (get-time-add minutes)
              (or virtual false))]
    (if ssl
      url
      (str "http" (subs url 5)))))

;(println
;  (with-s3 auth
;    (get-expiring-url "comandi" "files.policonnect.it" 1 :virtual true)))

;(println
;  (with-s3 auth
;    (get-expiring-url "comandi" "files.policonnect.it" 1)))

;(defn get-expiring-put-url [name bucket minutes]
;  (. *s3* createSignedGetUrl "PUT" bucket name nil (java.util.HashMap.) (* 60 minutes)))

;(def auth {:secret-key "Y69...ezE" :access-key "AKIA...2A"})

;(with-s3 auth
;  (put-file! (java.io.File. "/home/federico/comandi")
;     "test.policonnect.it"))

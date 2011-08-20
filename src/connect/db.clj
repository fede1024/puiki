(ns connect.db
  (:use somnium.congomongo))

(mongo! :db "connect") 

(insert! :people
  {:_id "s123"
   :pwd "ciao"
   :name "robbo"
   :surname "boh"
   :roles ["user"]
   :job "student"
   :created-at (java.util.Date.)})

(insert! :people
  {:_id "s124"
   :pwd "ciao"
   :name "Ajeje"
   :surname "boh"
   :roles ["admin" "user"]
   :job "student"
   :created-at (java.util.Date.)})

;(insert! :channels
;  {:name "Poli Connect"
;   :description "Canale dedicato a Poli Connect"
;   :posts 1
;   :created-at (java.util.Date.)})
;
;(insert! :posts
;  {:title "Benvenuto"
;   :content "Questo è un post di prova."
;   :type :normal
;   :channel (:_id (fetch-one :channels :where {:name "Poli Connect"}))
;   :created-at (java.util.Date.)})

(defn unref [db-ref]
  (when db-ref
    (somnium.congomongo.coerce/coerce (.fetch ^com.mongodb.DBRef db-ref)
      [:mongo :clojure])))

(defn obj-id [str]
  (try 
    (object-id str)
    (catch Exception _ nil)))

(defn ensure-exists [collection query]
  (or (fetch-one collection :where query)
    (throw (Exception. (str "Can't find object in " collection " where " query)))))

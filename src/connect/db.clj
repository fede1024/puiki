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

(fetch-one :people :where {:_id "s123"})

(some #{"admin"} (:roles (fetch-one :people :where {:_id "s123"})))

(defn unref [db-ref]
  (when db-ref
    (somnium.congomongo.coerce/coerce (.fetch ^com.mongodb.DBRef db-ref)
      [:mongo :clojure])))

(defn ensure-exists [collection query]
  (or (fetch-one collection :where query)
    (throw (Exception. (str "Can't find object in " collection " where " query)))))

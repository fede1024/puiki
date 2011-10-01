(ns connect.db
  (:use somnium.congomongo))

;(mongo! :db "connect")

(defn reset-db []
  (destroy! :people {})
  (destroy! :fields {})
  (destroy! :channels {})
  (destroy! :posts {}))

(defn init-db []
  (when (not (fetch-one :channels :where {:name "Poli Connect"}))
    (insert! :channels
      {:name "Poli Connect" :description "Canale dedicato a Poli Connect"
       :type :group :posts 1
       :created-at (java.util.Date.)})
    (insert! :posts
      {:title "Benvenuto" :content "Questo è un post di prova."
       :author "admin"
       :type :normal
       :channel (:_id (fetch-one :channels :where {:name "Poli Connect"}))
       :created-at (java.util.Date.)}))  
;  (insert! :people
;    {:_id "s123"  :pwd "ciao"
;     :firstname "robbo"  :lastname "boh"
;     :roles ["user"]
;     :job "student"
;     :follows [(:_id (fetch-one :channels :where {:name "Poli Connect"}))]
;     :created-at (java.util.Date.)})
  (insert! :people
    {:_id "admin"   :pwd "admin"
     :firstname "Admin" :lastname "Poli Connect"
     :roles ["admin" "user"]
     :job "student"
     :follows [(:_id (fetch-one :channels :where {:name "Poli Connect"}))]
     :created-at (java.util.Date.)}))

;(insert! :channels
;  {:name "Poli Connect"
;   :description "Canale dedicato a Poli Connect"
;   :type :normal
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

(defn db-last-error []
  (fetch-one :$cmd :where {:getlasterror 1}))

(defn follow-channel [channel-id person-id] ;;TODO: qui è il posto giusto?
  (update! :people {:_id person-id}
    {:$addToSet {:follows channel-id}})
  (update! :channels {:_id channel-id}
    {:$inc {:followers 1}}))

(defn- restore-all-posts! []
  (doseq [id (map :_id (fetch :posts :where {:removed true}))]
    (update! :posts {:_id id}
      {:$set {:removed nil :removed-by nil}})))

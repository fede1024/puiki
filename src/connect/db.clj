(ns connect.db
  (:use connect.email
    somnium.congomongo
    [ring.middleware.session.store :as ringstore])
  (:import [java.util UUID]))

;(mongo! :db "connect")

;; TODO: completare
(defn reset-db []
  (destroy! :people {})
  (destroy! :fields {})
  (destroy! :channels {})
  (destroy! :posts {}))

(defn init-db []
  (set-email!
    :username "poli.connect0@gmail.com"
    :password "mettereunapasswordsqui"
    :server "smtp.gmail.com"
    :port 465
    :protocol "smtp"
    :ssl true)
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

(defn obj-id [str] ;; Controllare se è già un obj-id?
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
    {:$addToSet {:follows channel-id}}))

(defn count-followers [channel-id] ;;TODO: qui è il posto giusto?
  (fetch-count :people :where {:follows channel-id}))

(defn- restore-all-posts! []
  (doseq [id (map :_id (fetch :posts :where {:removed true}))]
    (update! :posts {:_id id}
      {:$set {:removed nil :removed-by nil}})))

(defn- reset-all-news! []
  (update! :people {} ;; Cancella tutte le notifiche
    {:$unset {:news 1}} :multiple true))

;; 
;; SESSIONI
;; https://github.com/Raynes/mongo-session/blob/develop/src/mongo_session/core.clj
;;

(defn new-key []
  (str (UUID/randomUUID)))

;; Effettua molte richieste inutili al database
(defrecord MongoSessionStore [coll]
  SessionStore
  (read-session [this key]
    (let [data (:data (fetch-one coll :where {:_id key} :only [:data]))]
      ;(println "Leggo da" key data)
      (or (reduce merge (map (fn [[k val]] {(name k) val}) data)){})))
  (write-session [this key data]
    ;(println "Scrivo in " key (pr-str data))
    (when key
      ;(println "FATTO")
      (update! coll {:_id key}
        {:$set {:data data :last-access (java.util.Date.)}}))
    (or key (new-key)))
  (delete-session [this key]
    (destroy! coll {:_id key})
    nil))

(defn mongo-session
  "Create a mongodb store for ring sessions. The store will use whatever
  mongodb connection you have open, and assumes that it has ownership of the
  collection named by coll."
  [coll]
  (MongoSessionStore. coll))

(defn list-session-ids [store]
  (map :_id (fetch (:coll store) :only [:_id])))

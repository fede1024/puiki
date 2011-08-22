(ns connect.pages.post
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [clojure.contrib.string :as str]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(def post-images
  {:normal   [:img {:src "/images/asterisk-blue-small.png"}]
   :question [:img {:src "/images/question.png"}]
   :answer   [:img {:src "/images/exclamation.png"}]})

(defn update-vote [current dir]
  (or (get {[-1 "up"] 0  [0 "up"] 1  [1 "up"] 1
            [-1 "down"] -1  [0 "down"] -1 [1 "down"] 0}
        [current dir]) current))

;; TODO: controllo url malformati??
(defpage [:post "/user/vote/:id"] {:keys [id dir url]}
  (let [id (obj-id id)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (or (= dir "up") (= dir "down")))
      (let [my-vote (or (get (:votes post) (keyword (current-id))) 0)
            votes (merge (:votes post)
                    {(keyword (current-id)) (update-vote my-vote dir)})]
        (println my-vote votes)
        (update! :posts {:_id id}
          {:$set {:votes votes :vtotal (apply + (vals votes))}})
        (resp/redirect url))
      (render "/not-found"))))

(defpartial vote-section [post]
  (let [my-vote (or (when (current-id)
                      (get (:votes post) (keyword (current-id))))
                  0)
        tot (or (:vtotal post) 0)]
    [:span.vote
     (form-to [:post (post-vote-path post)]
       [:script {:type "text/javascript"}
        "document.write(\"<input type='hidden' name='url' value='\" + document.URL + \"'>\");"]
       (when (and (current-id) (< my-vote 1))
         [:button.vote {:name :dir :value "up"}
          [:img.vote {:height 16 :src "/images/up.png"}]])
       (when (and (current-id) (> my-vote -1))
         [:button.vote {:name :dir :value "down"}
          [:img.vote {:height 16 :src "/images/down.png"}]]))
     [:span.voteValue (str (when (> tot 0) "+") tot)]]))

(defn user-description [p]
  (cond (= (:job p) "student")
    (let [field (unref (:field p))]
      (when field
        (str " - " (:name field) " " (- 2012 (Integer/parseInt (:year p))) "° anno")));;TODO:fix
    (= (:job p) "professor") " - Docente"
    :else nil))

(defpartial post-table [post]
  [:table.post
   [:tr.postTitle
    [:td.postTitle (post-images (:type post)) " "
     (link-to (post-path post) (:title post))]
    [:td.postTitleLeft
     (when (= (:type post) :answer)
       (link-to (str "/post/" (:answer-to post)) "Domanda"))
     (vote-section post)]]
   [:tr.postInfo 
    (let [p (fetch-one :people :where {:_id (:author post)})]
      [:td.postAuthor "Postato da: " (:name p) " " (:surname p) (user-description p)])
    [:td.postDate (format-timestamp (:created-at post))]]
   [:tr.postContent
    [:td.postContent {:colspan 2} [:div.postContent [:pre (:content post)]]]]
   [:tr.postBottom
    [:td.postContent
     (link-to (post-path post)
       (when (= (:type post) :question)
         (str "Risposte: " (count (:answers post)) " "))
       ;(str "Commenti: " (count (db/get-comments (:id post))))
       )]
    [:td.postActions
     (when (= (:type post) :question)
       (form-to [:get (user-reply-path post)]
         (submit-button {:class "postReply"} "Rispondi")))
     (form-to [:get (user-comment-path post)]
       (submit-button {:class "postComment"} "Commenta"))]]])

;(defn error-table [description vd]
;  (when vd
;    [:table.error
;     [:tr.errorTitle [:td.errorTitle "Errore invio post"]]
;     (for [[k v] vd :when v] 
;       [:tr.errorBody
;        [:td.errorBody (description k)]])]))
;
;(def new-reply-errors 
;  {:content "Contenuto non valido"})

;(defn post-reply-table [post & [data vd]]
;  (form-to {:accept-charset "utf-8" } [:post (user-reply-path post)]
;    (error-table new-reply-errors vd)
;    [:table.post
;     [:tr.postTitle
;      [:td.postTitle {:colspan 2}
;       (text-field {:class :postTitle} :title
;         (if data (:title data)
;           (str "Risposta a: " (:title post))))]]
;     [:tr.postInfo 
;      (let [p (db/get-row :people :id (current-id))]
;        [:td.postAuthor "Postato da: " (:name p) " " (:surname p) (user-description p)])
;      [:td.postDate (format-timestamp (java.util.Date.))]]
;     [:tr.postContent
;      [:td.postContent {:colspan 2}
;       (text-area {:class :postContent :rows 15} :content
;         (:content data))]]
;     [:tr.postBottom
;      [:td.postActions {:colspan 2}
;       (submit-button {:class "postReply"} "Invia")]]]))
;
;(defn post-reply-page [post-id & [data vd]]
;  (layout "Rispondi"
;    (let [post (db/get-row :posts :id post-id :default nil)]
;      (if post
;        [:span
;         [:h2 "Post a cui stai rispondendo:"]
;         (post-table post)
;         [:h2 "Tuo intervento:"]
;         (post-reply-table post data vd)
;         (let [answers (sort-by #(apply + (vals (:votes %)))
;                         (map #(db/get-row :posts :id %) (:answers post)))]
;           [:span
;            [:h2 "Risposte precedenti: " (count answers)]
;            (for [answ answers]
;              (post-table answ))])]
;        [:p "Post non trovato"]))))


(defn post-page [id]
  (let [post (fetch-one :posts :where {:_id id})]
    (if post
      (layout "Post"
        (if (= :question (:type post))
          [:h2 "Domanda:"]
          [:h2 "Post:"])
        (post-table post)
        (when (= "question" (:type post))
          (let [answers (sort-by #(apply + (vals (:votes %))) >
                          (map #(fetch-one :posts :where {:_id %}) (:answers post)))]
            [:span
             [:h2 "Risposte: " (count answers)]
             (for [answ answers]
               (post-table answ))])))
      (render "/not-found"))))

(defpage "/user/new-post" [& [data]]
  (layout "Nuovo post"
    (let [person (fetch-one :people :where {:_id (current-id)})
          channels (concat '("--- Seguiti ---")
                     (map #(:name (unref %)) (:follows person))
                     '("--- Tutti ---")
                     (map :name (fetch :channels)))]
      (form-to {:accept-charset "utf-8" } [:post "/user/new-post"]
        ;(error-table new-post-errors vd)
        [:table.post
         [:tr.postTitle
          [:tr.postTitle
           [:td.postTitle {:colspan 2}
            (text-field {:class :postTitle} :title
              (if data (:title data) "Titolo post"))]]]
         [:tr.postInfo 
          (let [p (fetch-one :people :where {:_id (current-id)})]
            [:td.postAuthor "Postato da: " (:name p) " " (:surname p)
             (user-description p)])
          [:td.postDate (format-timestamp (java.util.Date.))]]
         [:tr.postContent
            [:td.postContent {:colspan 2}
             (text-area {:class :postContent :rows 15} :content
               (:content data))]]
         [:tr.postBottom
          [:td.postSettings {:colspan 2} "Tipo post: "
           [:input {:type :radio :name "type" :value ":question" :checked "true"} 
               "Normale"]
           [:input {:type :radio :name "type" :value ":question"} 
               "Domanda"]]]
         [:tr.postBottom
          [:td.postSettings
           "Canale: " (drop-down :channel channels)]
          [:td.postActions
           (submit-button {:class "postSubmit"} "Invia")]]]))))

(defn valid? [{:keys [title content channel type]}]
  (vali/rule (not (str/blank? title))
    [:title "Titolo non valido"])
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :channels :where {:name channel})
    [:channel "Canale non valido"]) ;;TODO: controllo sul tipo
  (not (vali/errors? :title :content :channel)))

(defpage [:post "/user/new-post"] {:keys [] :as post}
  (if (valid? post)
    (let [channel (fetch-one :channels :where {:name (:channel post)})]
      (insert! :posts
        (merge post {:author (current-id) :channel (:_id channel)
                     :created-at (java.util.Date.)}))
      (update! :channels {:_id (:_id channel)}
        {:$inc {:posts 1}})
      (resp/redirect (channel-path channel)))
    (render "/user/new-post" post)))


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

(pre-route "/edit*" p
  (if (current-id)
    (if (not (user? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (:uri p)})))

(defn update-vote [current dir]
  (or (get {[-1 "up"] 0  [0 "up"] 1  [1 "up"] 1
            [-1 "down"] -1  [0 "down"] -1 [1 "down"] 0}
        [current dir]) current))

;; TODO: controllo url malformati??
(defpage [:post "/edit/vote/:id"] {:keys [id dir url]}
  (let [id (obj-id id)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (or (= dir "up") (= dir "down")) (current-id))
      (let [my-vote (or (get (:votes post) (keyword (current-id))) 0)
            votes (merge (:votes post)
                    {(keyword (current-id)) (update-vote my-vote dir)})]
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
       (current-url-hidden)
       (when (and (current-id) (< my-vote 1))
         [:button.vote {:name :dir :value "up"}
          [:img.vote {:height 16 :src "/images/up.png"}]])
       (when (and (current-id) (> my-vote -1))
         [:button.vote {:name :dir :value "down"}
          [:img.vote {:height 16 :src "/images/down.png"}]]))
     [:span.voteValue (str (when (> tot 0) "+") tot)]]))

(defn user-description [p]
  (cond (= (:job p) "student")
    (str (:firstname p) " " (:lastname p)
      (when (:field p) (str " (" (:field p) " " (- 2012 (:year p)) "° anno)"))) ;;TODO:fix
    (= (:job p) "professor") " (Docente)"
    :else nil))

(def post-images
  {"normal"   [:img {:src "/images/asterisk-blue-small.png"}]
   "question" [:img {:src "/images/question.png"}]
   "answer"   [:img {:src "/images/exclamation.png"}]})

(defpartial post-bottom [post]
  (if (= (session/get :edit-post-comment) (str (:_id post)))
    (form-to {:accept-charset "utf-8" } [:get (user-comment-path post)] ;; Invio commento
      (current-url-hidden)
      [:tr.postBottom
       [:td.postContent {:colspan 2}
        (text-area {:class :postComment :rows 1 :maxlength 300 :placeholder "Commento"} :comment)]]
      [:tr.postBottom  
       [:td.postContent
        "Massimo 300 caratteri."]
       [:td.postActions
        (submit-button {:class "postComment" :name :undo} "Annulla")
        (submit-button {:class "postComment"} "Commenta")]])
    [:tr.postBottom     ;; Rispondi/Commenta
     [:td.postContent
      (when (= (:type post) "answer")
        (html (link-to (post-path (fetch-one :posts :where {:_id (:answers-to post)}))
                "Domanda") " "))
      (link-to (post-path post)
        (when (= (:type post) "question")
          (str "Risposte: " (or (:answers post) 0))))
      (str " Commenti: " (or (:comments-num post) 0))]
     [:td.postActions
      (when (= (:type post) "question")
        (form-to [:get (user-reply-path post)]
          (submit-button {:class "postReply"} "Rispondi")))
      (form-to [:get (user-comment-path post)]
        (current-url-hidden)
        (submit-button {:class "postComment"} "Commenta"))]]))

(defn post-comments [post] ;;; TODO: fix qui
  (for [{body :body author :autor date :created-at} (:comments post)]
    (html
      [:tr.commentInfo
       (let [p (fetch-one :people :where {:_id author})]
         [:td.commentAuthor (user-description p) " scrive:"])
       [:td.commentDate (format-timestamp date)]]
      [:tr.commentBody
       [:td.commentBody {:colspan 2} [:div.commentBody body]]])))

(defn post-comments [post]
  (for [{body :body author :author date :created-at} (:comments post)]
    [:tr.comment
     [:td.comment {:colspan 2} body
      (let [p (fetch-one :people :where {:_id author})]
        [:span.commentInfo (user-description p) " " (format-timestamp date)])]]))

(defpartial post-table [post & {:keys [show-remove] :or {show-remove true}}]
  [:div.post
   (if (:removed post)
     (if (:removed-by post)
       (if (= (:removed-by post) (:author post))
         [:div.remMsg "Post rimosso dall'autore."]
         (let [p (fetch-one :people :where {:_id (:removed-by post)})]
           [:div.remMsg "Post rimosso da: " (user-description p)]))
       [:div.remMsg "Post rimosso."])
     [:table.post
      [:tr.postTitle
       [:td.postTitle (post-images (:type post)) " "
        (link-to {:class :postTitle} (post-path post) (:title post))]
       [:td.postTitleLeft
        (vote-section post)
        (when (and show-remove (current-id) 
                (or (admin? (current-id)) (= (current-id) (:author post))))
          (form-to [:post (post-remove-path post)]
            (current-url-hidden)
            (submit-button {:class "postRemove"} "Cancella")))]]
      [:tr.postInfo 
       (let [p (fetch-one :people :where {:_id (:author post)})]
         [:td.postAuthor "Postato da: " (user-description p)])
       [:td.postDate (format-timestamp (:created-at post))]]
      [:tr.postContent
       [:td.postContent {:colspan 2} [:div.postContent [:pre (:content post)]]]]
      (post-comments post)
      (post-bottom post)])])

(defpartial post-summary [post]
  [:h2 "Informazioni post:"]
  [:p "Commenti: " (count (:comments post))] ;; TODO: fix?
  (when (= "question" (:type post))
    [:p "Risposte: " (fetch-count :posts :where {:answers-to (:_id post)})])
  (when (= (:type post) "answer")
    [:p (link-to (post-path (fetch-one :posts :where {:_id (:answers-to post)}))
          "Domanda")]))

(defpartial channel-link [post]
  (let [ch (fetch-one :channels :where {:_id (:channel post)})]
    [:p "Canale: " (link-to (channel-path ch) (:name ch))]))

(defpage "/post/:id" {:keys [id]}
  (let [id (obj-id id)
        post (fetch-one :posts :where {:_id id})]
    (if post
      (binding [*sidebar* (html (post-summary post)
                            (channel-link post))]
        (layout "Post"
          (if (= "question" (:type post))
            [:h2 "Domanda:"]
            [:h2 "Post:"])
          (post-table post)
          (when (= "question" (:type post))
            (let [answers (sort-by #(or (:vtotal %) 0) >
                            (fetch :posts :where {:answers-to id}))]
              [:span
               [:h2 "Risposte: " (count answers)]
               (for [answ answers]
                 (post-table answ))]))))
      (render "/not-found"))))

;; TODO: fix selezione canale (farlo scegliere davvero qui??)
(defpage "/edit/new-post" {:keys [title content channel channel-id type]}
  (layout "Nuovo post"
    (let [person (fetch-one :people :where {:_id (current-id)})
          channels (concat (when channel-id
                             (list (:name (fetch-one :channels :where {:_id (obj-id channel-id)}))))
                     '("--- Seguiti ---")
                     (map #(:name (fetch-one :channels :where {:_id %}))
                       (:follows person))
                     '("--- Tutti ---")
                     (map :name (fetch :channels)))]
      (form-to {:accept-charset "utf-8" } [:post "/edit/new-post"]
        (error-table "Errore invio post")
        [:div.post
         [:table.post
          [:tr.postTitle
           [:tr.postTitle
            [:td.postTitle {:colspan 2}
             (text-field {:class :postTitle :placeholder "Titolo post"} :title
               title)]]]
          [:tr.postInfo 
           [:td.postAuthor "Postato da: " (user-description person)]
           [:td.postDate (format-timestamp (java.util.Date.))]]
          [:tr.postContent
           [:td.postContent {:colspan 2}
            (text-area {:class :postContent :rows 15
                        :placeholder "Contenuto del post"}
              :content content)]]
          [:tr.postBottom
           [:td.postSettings {:colspan 2} "Tipo post: "
            [:input {:type :radio :name "type" :value "normal"
                     :checked (when (or (not type) (= type "normal")) "true")} 
             "Normale"]
            [:input {:type :radio :name "type" :value "question"
                     :checked (when (= type "question") "true")}
             "Domanda"]]]
          [:tr.postBottom
           [:td.postSettings
            "Canale: " (drop-down :channel channels)]
           [:td.postActions
            (submit-button {:class "postSubmit"} "Invia")]]]]))))

(defn valid-post? [{:keys [title content channel type]}]
  (vali/rule (not (str/blank? title))
    [:title "Titolo non valido"])
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :channels :where {:name channel})
    [:channel "Canale non valido"]) ;;TODO: controllo sul tipo
  (not (vali/errors? :title :content :channel)))

(defpage [:post "/edit/new-post"] {:as post}
  (if (valid-post? post)
    (let [channel (fetch-one :channels :where {:name (:channel post)})]
      (insert! :posts
        (merge post {:author (current-id) :channel (:_id channel)
                     :created-at (java.util.Date.)}))
      (update! :channels {:_id (:_id channel)}
        {:$inc {:posts 1}})
      (resp/redirect (channel-path channel)))
    (render "/edit/new-post" post)))

(defpage [:post "/edit/remove/:pid"] {:keys [pid confirmed url]}
  (let [id (obj-id pid)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (current-id) 
          (or (admin? (current-id)) (= (current-id) (:author post))))
      (if confirmed
        (do (update! :posts {:_id id}
              {:$set {:removed true :removed-by (current-id)}})
          (resp/redirect url))
        (layout "Conferma"
          [:h2 "Conferma cancellazione post"]
          [:p "Sei sicuro di voler cancellare il post?"]
          (form-to [:post (post-remove-path post)]
            [:input {:type :hidden :name :url :value url}]
            [:input {:type :hidden :name :confirmed :value :true}]
            (submit-button {:class "confirm"} "Conferma"))
          (form-to [:get url]
            (submit-button {:class "abort"} "Annulla"))
          [:h2 "Post da rimuovere:"]
          (post-table post :show-remove false)))
      (render "/not-found"))))

(defpage [:get "/edit/comment/:pid"] {:keys [pid comment undo url]}
  (when (current-id)
    (if (or undo comment)
      (session/remove! :edit-post-comment)
      (session/put! :edit-post-comment pid))
    (when (and comment (not undo))
      (update! :posts {:_id (obj-id pid)}
        {:$push {:comments {:body comment :author (current-id)
                            :created-at (java.util.Date.)}}
         :$inc {:comments-num 1}})))
  (resp/redirect url))

(defpartial post-reply-table [question & [reply]]
  (form-to {:accept-charset "utf-8" } [:post (user-reply-path question)]
    (error-table "Errore invio post")
    [:div.post
     [:table.post
      [:tr.postTitle
       [:td.postTitle {:colspan 2}
        ;(post-images "answer") " "
        (text-field {:class :postTitle} :title
          (or (:title reply) (str "Risposta a: " (:title question))))]]
      [:tr.postInfo 
       (let [p (fetch-one :people :where {:_id (current-id)})]
         [:td.postAuthor "Postato da: " (user-description p)])
       [:td.postDate (format-timestamp (java.util.Date.))]]
      [:tr.postContent
       [:td.postContent {:colspan 2}
        (text-area {:class :postContent :rows 15 :placeholder "Contenuto del post"}
          :content (:content reply))]]
      [:tr.postBottom
       [:td.postActions {:colspan 2}
        (submit-button {:class "postReply"} "Invia")]]]]))

(defpage "/edit/reply/:qid" {:keys [qid] :as reply}
  (layout "Rispondi"
    (let [qid (obj-id qid)
          question (fetch-one :posts :where {:_id qid})]
      (if (and question (not (:removed question)))
        [:span
         [:h2 "Tuo intervento:"]
         (post-reply-table question reply)
         [:h2 "Post a cui stai rispondendo:"]
         (post-table question)
         (let [answers (fetch :posts :where {:answers-to qid} :sort {:vtotal -1})]
           [:span
            [:h2 "Risposte precedenti: " (count answers)]
            (for [answ answers]
              (post-table answ))])]
        [:p "Post non trovato"]))))

(defn valid-reply? [{:keys [qid title content]}]
  (vali/rule (not (str/blank? title))
    [:title "Titolo non valido"])
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :posts :where {:_id (obj-id qid)})
    [:post "Non esiste un post di domanda"])
  (not (vali/errors? :title :content)))

(defpage [:post "/edit/reply/:qid"] {:keys [qid] :as reply}
  (if (valid-reply? reply)
    (let [qid (obj-id qid)
          question (fetch-one :posts :where {:_id qid})]
      (insert! :posts
        {:title      (:title reply)    :content    (:content reply)
         :author     (current-id)      :channel    (:channel question)
         :created-at (java.util.Date.) :answers-to qid
         :type       "answer"})
      (update! :channels {:_id (:channel question)} {:$inc {:posts 1}})
      (update! :posts {:_id qid} {:$inc {:answers 1}})
      (resp/redirect (post-path question)))
    (render "/edit/reply/:qid" reply)))

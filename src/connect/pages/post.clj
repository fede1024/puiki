(ns connect.pages.post
  (:use connect.pages.layout
        connect.pages.utils
        connect.db
        connect.search
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [clojure.contrib.string :as str]
           [clojure.contrib.json :as json]
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(pre-route "/edit*" request
  (if (current-id)
    (if (not (user? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (get-request-uri request)})))

(defn js-do [& objs]
  (apply str objs))

(defn js-show [& objs]
  (str "$('" (apply str objs) "').css('display', '');"))

(defn js-hide [& objs]
  (str "$('" (apply str objs) "').css('display', 'none');"))

(defn js-get [path id opts]
  (str "$('#loader').css('display', 'inline');"
    "$.get('" path "', " (clojure.contrib.json/json-str opts) ", function(content) {$('#" id "').html(content);"
    "$('#loader').css('display', 'none');});"))

(defn js-post [path id opts]
  (str "$('#loader').css('display', 'inline');"
    "$.post('" path "', " (clojure.contrib.json/json-str opts) ", function(content) {$('#" id "').html(content);"
    "$('#loader').css('display', 'none');});"))

(defn js-vote [post dir]
  (js-get (post-vote-path post) (:_id post) {:dir dir}))

(defpartial vote-section [post]
  (let [my-vote (or (when (current-id)
                      (get (:votes post) (keyword (current-id))))
                  0)
        tot (or (:vtotal post) 0)]
    [:span.vote
     (if (and (current-id) (< my-vote 1))
       [:button.vote {:name :dir :value "up" :onClick (js-vote post :up)}
        [:img.vote {:height 16 :src "/images/up.png"}]]
       [:button.vote {:disabled true}
        [:img.vote {:height 16 :src "/images/up-off.png"}]])
     (if (and (current-id) (> my-vote -1))
       [:button.vote {:name :dir :value "down" :onClick (js-vote post :down)}
        [:img.vote {:height 16 :src "/images/down.png"}]]
       [:button.vote {:disabled true}
        [:img.vote {:height 16 :src "/images/down-off.png"}]])
     [:span.voteValue (str (when (> tot 0) "+") (when (= tot 0) " ") tot)]]))

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

(defn js-comment [post]
  (str "$('#loader').css('display', 'inline');"
    "$.post('" (user-comment-path post) "', {comment: $('#commentText"  (:_id post) "').val()}, function(content) {$('#" (:_id post) "').html(content);"
    "$('#loader').css('display', 'none');});"))

(defpartial post-bottom [post]
  [:tr.postBottom {:id (str "commentArea" (:_id post)) :style "display: none"}
   [:td.postContent {:colspan 2}
    (text-area {:id (str "commentText" (:_id post))
                :class :postComment :rows 1 :maxlength 300 :placeholder "Commento"} :comment)]]
  [:tr.postBottom {:id (str "commentButtons" (:_id post)) :style "display: none"}
   [:td.postContent 
    "Massimo 300 caratteri."]
   [:td.postActions
    [:button {:class "postComment"  :onClick (js-do (js-hide "#commentArea" (:_id post))
                                               (js-hide "#commentButtons" (:_id post))
                                               (js-show "#actions" (:_id post)))}
     "Annulla"]
    [:button {:class "postComment" :onClick (js-comment post)} "Commenta"]]]
  [:tr.postBottom {:id (str "actions" (:_id post))}    ;; Rispondi/Commenta
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
    [:button (merge {:class "postComment" :onClick (js-do (js-hide "#actions" (:_id post))
                                                     (js-show "#commentArea" (:_id post))
                                                     (js-show "#commentButtons" (:_id post)))}
               (if (not (current-id)) {:disabled true}))
     "Commenta"]]])

(defn post-comments [post]
  (for [{body :body author :author date :created-at} (:comments post)]
    [:tr.comment
     [:td.comment {:colspan 2} body
      (let [p (fetch-one :people :where {:_id author})]
        [:span.commentInfo (user-description p) " " (format-timestamp date)])]]))

(defpartial post-table [post & {:keys [show-remove] :or {show-remove true}}]
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
         [:button {:class "postRemove" :onClick (js-post (post-remove-path post) (:_id post) {})}
          "Cancella"])]]
     [:tr.postInfo 
      (let [p (fetch-one :people :where {:_id (:author post)})]
        [:td.postAuthor "Postato da: " (user-description p)])
      [:td.postDate (format-timestamp (:created-at post))]]
     [:tr.postContent
      [:td.postContent {:colspan 2}
       [:div.postContent (:content post)]]]
     (post-comments post)
     (post-bottom post)]))

(defpartial post-div [post & {:keys [show-remove] :or {show-remove true}}]
  [:div.post {:id (:_id post)}
   (post-table post :show-remove show-remove)])

(defpartial post-summary [post]
  [:h2.section "Informazioni post:"]
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
            [:h2.section "Domanda:"]
            [:h2.section "Post:"])
          (post-div post)
          (when (= "question" (:type post))
            (let [answers (sort-by #(or (:vtotal %) 0) >
                            (fetch :posts :where {:answers-to id}))]
              [:span
               [:h2.section "Risposte: " (count answers)]
               (for [answ answers]
                 (post-div answ))]))))
      (render "/not-found"))))

(defn update-vote [current dir]
  (or (get {[-1 "up"] 0  [0 "up"] 1  [1 "up"] 1
            [-1 "down"] -1  [0 "down"] -1 [1 "down"] 0}
        [current dir]) current))

(defpage [:get "/edit/vote/:id"] {:keys [id dir url]}
  (let [id (obj-id id)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (or (= dir "up") (= dir "down")) (current-id))
      (let [my-vote (or (get (:votes post) (keyword (current-id))) 0)
            votes (merge (:votes post)
                    {(keyword (current-id)) (update-vote my-vote dir)})]
        (update! :posts {:_id id}
          {:$set {:votes votes :vtotal (apply + (vals votes))}})
        (post-table (fetch-one :posts :where {:_id id})))
      "ERRORE")))

(def tinymce-header
  (html
    [:script {:type "text/javascript" :src "/tinymce/jscripts/tiny_mce/tiny_mce.js"}]
    [:script {:type "text/javascript" :src "/tinymce.js"}]))

(defpage "/edit/new-post" {:keys [title content channel-id type]}
  (binding [*custom-header* tinymce-header]
    (layout "Nuovo post"
      (let [person (fetch-one :people :where {:_id (current-id)})]
        (form-to {:accept-charset "utf-8" } [:post "/edit/new-post"]
          (error-table "Errore invio post")
          [:div.post
           [:table.post
            [:tr.postTitle
             [:td.postTitle {:colspan 2}
              (text-field {:class :postTitle :placeholder "Titolo post"} :title
                title)]]
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
             [:td.postSettings "Canale: "
              (when channel-id
                (let [channel (fetch-one :channels :where {:_id (obj-id channel-id)})]
                  (html [:input {:type :hidden :name :channel-id :value channel-id}]
                    (link-to (channel-path channel) (:name channel)))))]
             [:td.postActions
              (submit-button {:class "postSubmit"} "Invia")]]]])))))

(defn valid-post? [{:keys [title content channel-id type]}]
  (vali/rule (not (str/blank? title))
    [:title "Titolo non valido"])
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :channels :where {:_id (obj-id channel-id)})
    [:channel-id "Canale non valido"]) ;;TODO: controllo sul tipo
  (not (vali/errors? :title :content :channel-id)))

(defpage [:post "/edit/new-post"] {:as post}
  (if (current-id)
    (if (valid-post? post)
      (let [c-id (obj-id (:channel-id post))
            ch (fetch-one :channels :where {:_id c-id})]
        (insert! :posts
          (merge post {:author (current-id) :channel c-id
                       :created-at (java.util.Date.)
                       :keywords (post-keywords post)}))
        (update! :channels {:_id c-id}
          {:$inc {:posts 1}})
        (resp/redirect (channel-path ch)))
      (render "/edit/new-post" post))
    (resp/redirect "/login")))

(defpage [:post "/edit/remove/:pid"] {:keys [pid conf undo]}
  (let [id (obj-id pid)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (current-id) (not undo)
          (or (admin? (current-id)) (= (current-id) (:author post))))
      (if conf
        (do (update! :posts {:_id id}
              {:$set {:removed true :removed-by (current-id)}})
          (post-table (fetch-one :posts :where {:_id id})))
        (html
          [:p "Sei sicuro di voler cancellare il post?"
           [:button {:class "continue"
                     :onClick (js-post (post-remove-path post) (:_id post) {:conf true})}
            "Conferma"]
           [:button {:class "abort"
                     :onClick (js-post (post-remove-path post) (:_id post) {:undo true})}
           "Annulla"]]
          (post-table post)))
      (post-table post))))

(defpage [:post "/edit/comment/:pid"] {:keys [pid comment]}
  (when (and (current-id) (not (str/blank? comment)))
    (update! :posts {:_id (obj-id pid)}
      {:$push {:comments {:body comment :author (current-id)
                          :created-at (java.util.Date.)}}
       :$inc {:comments-num 1}}))
  (post-table (fetch-one :posts :where {:_id (obj-id pid)})))

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
  (binding [*custom-header* tinymce-header]
    (layout "Rispondi"
      (let [qid (obj-id qid)
            question (fetch-one :posts :where {:_id qid})]
        (if (and question (not (:removed question)))
          [:span
           [:h2.section "Tuo intervento:"]
           (post-reply-table question reply)
           [:h2.section "Post a cui stai rispondendo:"]
           (post-div question)
           (let [answers (fetch :posts :where {:answers-to qid} :sort {:vtotal -1})]
             [:span
              [:h2.section "Risposte precedenti: " (count answers)]
              (for [answ answers]
                (post-div answ))])]
          [:p "Post non trovato"])))))

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
         :type       "answer"
         :keywords   (post-keywords reply)})
      (update! :channels {:_id (:channel question)} {:$inc {:posts 1}})
      (update! :posts {:_id qid} {:$inc {:answers 1}})
      (resp/redirect (post-path question)))
    (render "/edit/reply/:qid" reply)))

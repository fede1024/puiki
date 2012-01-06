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
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]))

(pre-route "/edit*" request
  (if (current-id)
    (if (not (user? (current-id)))
      (render "/permission-denied"))
    (render "/login" {:redirect (get-request-uri request)})))

(defn js-vote [post dir]
  (js-get (post-vote-path post) (str "votes" (:_id post)) {:dir dir}))

(defpartial vote-section [post]
  (let [my-vote (or (when (current-id)
                      (get (:votes post) (keyword (current-id))))
                  0)
        tot (or (:vtotal post) 0)]
    [:span.vote {:title "Vota il post"}
     (if (and (current-id) (< my-vote 1))
       [:a.vote {:onClick (js-vote post :up)}
        [:img.vote {:height 16 :src "/images/up.png"}]]
       [:a.vote {:disabled true}
        [:img.vote {:height 16 :src "/images/up-off.png"}]])
     (if (and (current-id) (> my-vote -1))
       [:a.vote {:onClick (js-vote post :down)}
        [:img.vote {:height 16 :src "/images/down.png"}]]
       [:a.vote {:disabled true}
        [:img.vote {:height 16 :src "/images/down-off.png"}]])
     [:span.voteValue {:title (str "+" (count (filter #(= (second %) 1) (:votes post)))
                                " -" (count (filter #(= (second %) -1) (:votes post))))}
      (str (when (> tot 0) "+") (when (= tot 0) " ") tot)]]))

(defpartial user-description [p & {:keys [field] :or {field true}}]
  (str (:firstname p) (when (current-id) (str " " (:lastname p)))
    (when (and field (:field p))
      (str " (" (:field p) " " (- 2012 (:year p)) "°anno)"))))

(def post-images
  {"normal"   [:img.middle {:src "/images/page-big.png"}]
   "question" [:img.middle {:src "/images/question-big.png"}]
   "answer"   [:img.middle {:src "/images/exclamation.png"}]})

(defn js-comment [post]
  (str "$('#loader').css('display', 'inline');"
    "$.post('" (user-comment-path post) "', {comment: $('#commentText"  (:_id post) "').val()}, function(content) {$('#comments" (:_id post) "').html(content);"
    "$('#loader').css('display', 'none');});"
    "$('#commentText"  (:_id post) "').val('');"
    (js-hide "#commentArea" (:_id post))
    (js-hide "#commentButtons" (:_id post))
    (js-show "#actions" (:_id post))))

(defpartial post-bottom [post]
  [:tr.postBottom {:id (str "commentArea" (:_id post)) :style "display: none"}
   [:td.postInfo {:colspan 2}
    (text-area {:id (str "commentText" (:_id post))
                :class :postComment :rows 1 :maxlength 300 :placeholder "Commento"} :comment)]]
  [:tr.postBottom {:id (str "commentButtons" (:_id post)) :style "display: none"}
   [:td.postInfo 
    "Massimo 300 caratteri."]
   [:td.postActions
    [:button {:class "postComment" :onClick (js-do (js-hide "#commentArea" (:_id post))
                                              (js-hide "#commentButtons" (:_id post))
                                              (js-show "#actions" (:_id post)))}
     "Annulla"]
    [:button {:class "postComment" :onClick (js-comment post)} "Commenta"]]]
  [:tr.postBottom {:id (str "actions" (:_id post))}    ;; Rispondi/Commenta
   [:td.postInfo]
   [:td.postActions
    (when (= (:type post) "question")
      (form-to [:get (user-reply-path post)]
        (submit-button {:class "postReply"} "Rispondi")))
    [:button (merge {:class "postComment" :onClick (js-do (js-hide "#actions" (:_id post))
                                                     (js-show "#commentArea" (:_id post))
                                                     (js-show "#commentButtons" (:_id post)))}
               (if (not (current-id)) {:disabled true}))
     "Commenta"]]])

(defpartial post-comments [post]
  [:table
   (for [{body :body author :author date :created-at} (:comments post)]
     [:tr.comment
      (let [p (fetch-one :people :where {:_id author})]
        [:td.comment
         [:span.commentInfo {:title (str (user-description p) " " (format-timestamp-relative date))}
          (user-description p :field false)]
         body])])])

(defpartial remove-button [post & [confirm]]
  (let [div-id (str "remove" (:_id post))]
    (if confirm
      [:span
       [:button.continue {:onClick (js-post (post-remove-path post) div-id {:conf true})
                          :title "Conferma cancellazione"}
        "Conferma"] " "
       [:button.abort {:onClick (js-post (post-remove-path post) div-id {:undo true})
                       :title "Annulla cancellazione"}
        "Annulla"]]
      [:a.postRemove {:onClick (js-post (post-remove-path post) div-id {})
                      :title "Cancella il post"}
       [:img.remove {:src "/images/remove.png"}]])))

(defpartial post-table [post & {:keys [preview]}]
  (if (:removed post)
    (if (:removed-by post)
      (if (= (:removed-by post) (:author post))
        [:div.remMsg "Post rimosso dall'autore."]
        (let [p (fetch-one :people :where {:_id (:removed-by post)})]
          [:div.remMsg "Post rimosso da: " (user-description p)]))
      [:div.remMsg "Post rimosso."])
    (html
      (when (not (= (:type post) "answer"))
        [:h2.postTitle (post-images (:type post)) " " (:title post)])
      [:table.post
       [:tr
        (let [p (fetch-one :people :where {:_id (:author post)})]
          [:td.postAuthor (user-description p)])
        [:td.postEdit {:rowspan 2}
         [:a {:href (post-path post)}
          [:img.edit {:src "/images/link.png" :alt "Link" :title "Link permanente"}]]
         (when (and (not preview) (current-id) 
                 (or (admin? (current-id)) (= (current-id) (:author post))))
           [:span {:id (str "remove" (:_id post))}
            (remove-button post)])
         (when (not preview)
           [:span {:id (str "votes" (:_id post))}
            (vote-section post)])]]
       [:tr [:td.postDate (format-timestamp-relative (:created-at post))]]
       [:tr [:td.postContent {:colspan 2}
             (if (= (:type post) "normal")
               [:div.pageContent (:content post)]
               [:div.questionContent (:content post)])]]
       (when (not preview)
         [:tr [:td {:id (str "comments" (:_id post))}
               (post-comments post)]])
       (when (not preview)
         (post-bottom post))])))

(defpartial post-div [post & {:keys [preview]}]
  [:div.post.anchor {:id (str "post" (:_id post))}
   (post-table post :preview preview)])

(defpartial post-summary [post]
  [:h2.section "Informazioni post:"]
  [:p "Commenti: " (count (:comments post))] ;; TODO: fix?
  (when (= "question" (:type post))
    [:p "Risposte: " (fetch-count :posts :where {:answers-to (:_id post)})]))

(defpartial channel-link [post]
  (let [ch (fetch-one :channels :where {:_id (:channel post)})]
    [:p "Canale: " (link-to (channel-path ch) (:name ch))]))

(defpartial post-answers [post]
  (when (= "question" (:type post))
    (let [answers (sort-by #(or (:vtotal %) 0) >
                    (fetch :posts :where {:answers-to (:_id post)}))
          n (count answers)]
      (html
        [:h2.answers "Risposte: " (if (zero? n) "nessuna" n)]
        (for [answ (butlast answers)]
          (html
            (post-div answ)
            [:hr.answer]))
        (when (not (empty? answers))
          (post-div (last answers)))))))

(defpage "/post/:id" {:keys [id]}
  (let [id (obj-id id)
        post (fetch-one :posts :where {:_id id})]
    (if post
      (binding [*sidebar* (html [:div.sideBarSection
                                   (post-summary post)
                                   (channel-link post)])]
        (when (current-id)
          (update! :people {:_id (current-id)} ;; Toglie il post dalle notifiche
            {:$pull {:news {:post id}}}
            :multiple true)
          (when (= (:type post) "question")
            (dorun
              (for [answer (fetch :posts :where {:answers-to id})]
                (update! :people {:_id (current-id)} ;; tolgo le risposte dalle notifiche
                  {:$pull {:news {:post (:_id answer)}}}
                  :multiple true)))))
        (layout (:title post)
          (let [ch (fetch-one :channels :where {:_id (:channel post)})]
            [:h1.section [:a.nodecor {:href (channel-path ch)} (:name ch) ":"]])
          (post-div post)
          (post-answers post)))
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
        (vote-section (fetch-one :posts :where {:_id id})))
      "ERRORE")))

(def ckeditor-header
  (html
    [:script {:type "text/javascript" :src "/ckeditor/ckeditor.js"}]
    [:script {:type "text/javascript" :src "/ckeditor-config.js"}]))

(def new-post-help
  (html
    [:p "Per inserire il codice sorgente clicca sul tasto code. Per inserire una formula "
     "matematica puoi utilizzare " [:a {:href "http://mathurl.com" :target "_blank"} "questo sito"]
     ". Una volta realizzata la formula clicca su \"make mathURL\" e inserisci qui la formula "
     [:em "come immagine"] " (usando l'indirizzo che trovi cliccando sulla formula col tasto destro "
     "e successivamente su \"Indirizzo immagine\")."]))

(defpage [:post "/edit/new-page"] {:keys [title content channel-id]}
  (binding [*custom-header* ckeditor-header]
    (let [person (fetch-one :people :where {:_id (current-id)})
          channel (fetch-one :channels :where {:_id (obj-id channel-id)})]
      (if (not (and person channel))
        (render "/permission-denied")
        (layout "Nuova pagina"
          [:h1.section "Crea una nuova pagina"]
          (form-to {:accept-charset "utf-8"} [:post "/edit/new-page/preview"]
            (error-table "Errore invio pagina")
            [:div.post
             [:table.post
              [:tr
               [:td.newPostTitle {:colspan 2} "Titolo: "
                (text-field {:class :postTitle :placeholder "Titolo Pagina"} :title
                  title)]]
              [:tr [:td.postAuthor (user-description person)]]
              [:tr [:td.postDate (format-timestamp-relative (java.util.Date.))]]
              [:tr.postContent
               [:td.postContent {:colspan 2}
                (text-area {:class :ckeditor :rows 15 :placeholder "Contenuto della pagina"}
                  :content content)]]
              [:tr.postBottom
               [:td.postSettings "Canale: "
                [:input {:type :hidden :name :channel-id :value channel-id}]
                (link-to (channel-path channel) (:name channel))]
               [:td.postActions
                (submit-button {:class "postSubmit"} "Anteprima e invio")]]]])
          new-post-help)))))

(defpage "/edit/new-page" {:keys [channel-id] :as data}
  (render [:post "/edit/new-page"] data))

(defpage [:post "/edit/new-question"] {:keys [title content channel-id]}
  (binding [*custom-header* ckeditor-header]
    (let [person (fetch-one :people :where {:_id (current-id)})
          channel (fetch-one :channels :where {:_id (obj-id channel-id)})]
      (if (not (and person channel))
        (render "/permission-denied")
        (layout "Nuova domanda"
          [:h1.section "Crea una nuova domanda"]
          (form-to {:accept-charset "utf-8"} [:post "/edit/new-question/preview"]
            (error-table "Errore invio domanda")
            [:div.post
             [:table.post
              [:tr
               [:td.newPostTitle {:colspan 2} "Titolo: "
                (text-field {:class :postTitle :placeholder "Titolo domanda"} :title
                  title)]]
              [:tr [:td.postAuthor (user-description person)]]
              [:tr [:td.postDate (format-timestamp-relative (java.util.Date.))]]
              [:tr.postContent
               [:td.postContent {:colspan 2}
                (text-area {:class :ckeditor :rows 15
                            :placeholder "Descrizione della domanda"}
                  :content content)]]
              [:tr.postBottom
               [:td.postSettings "Canale: "
                [:input {:type :hidden :name :channel-id :value channel-id}]
                (link-to (channel-path channel) (:name channel))]
               [:td.postActions
                (submit-button {:class "postSubmit"} "Anteprima e invio")]]]])
          new-post-help)))))

(defpage "/edit/new-question" {:keys [channel-id] :as data}
  (render [:post "/edit/new-question"] data))

(defn valid-post? [{:keys [title content channel-id]}]
  (vali/rule (not (str/blank? title))
    [:title "Titolo non valido"])
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :channels :where {:_id (obj-id channel-id)})
    [:channel-id "Canale non valido"])
  (not (vali/errors? :title :content :channel-id)))

(defpage [:post "/edit/new-page/preview"] {:as post}
  (if (current-id)
    (if (valid-post? post)
      (let [hiddens (for [[name value] post]
                      [:input {:type :hidden :name name :value value}])]
        (layout "Anteprima pagina"
          (post-div (merge post {:author (current-id) :channel (:channel-id post)
                                 :created-at (java.util.Date.)})
            :preview true)
          (form-to {:accept-charset "utf-8" } [:post "/edit/new-page"]
            hiddens
            (submit-button {:class "postSubmit"} "Modifica pagina"))
          (form-to {:accept-charset "utf-8" } [:post "/edit/save-page"]
            hiddens
            (submit-button {:class "postSubmit"} "Invia pagina"))))
      (render [:post "/edit/new-page"] post))
    (resp/redirect "/login")))

(defpage [:post "/edit/new-question/preview"] {:as post}
  (if (current-id)
    (if (valid-post? post)
      (let [hiddens (for [[name value] post]
                      [:input {:type :hidden :name name :value value}])]
        (layout "Anteprima domanda"
          (post-div (merge post {:author (current-id) :channel (:channel-id post)
                                 :created-at (java.util.Date.)})
            :preview true)
          (form-to {:accept-charset "utf-8" } [:post "/edit/new-question"]
            hiddens
            (submit-button {:class "postSubmit"} "Modifica domanda"))
          (form-to {:accept-charset "utf-8" } [:post "/edit/save-question"]
            hiddens
            (submit-button {:class "postSubmit"} "Invia domanda"))))
      (render [:post "/edit/new-question"] post))
    (resp/redirect "/login")))

(defn new-content! [post type]
  (if (current-id)
    (if (valid-post? post)
      (let [c-id (obj-id (:channel-id post))
            ch (fetch-one :channels :where {:_id c-id})
            new-post (insert! :posts
                       (dissoc
                         (merge post {:author (current-id) :channel c-id
                                      :created-at (java.util.Date.)
                                      :keywords (post-keywords post)
                                      :type type})
                         :channel-id))]
        (update! :channels {:_id c-id}
          {:$inc {:posts 1}})
        (update! :people {:follows c-id :_id {:$ne (current-id)}} ;; Update a tutti i followers
          {:$push {:news {:action :new-post :post (:_id new-post)
                          :channel c-id :time (java.util.Date.)}}}
          :multiple true :upsert false)
        (resp/redirect (post-path new-post)))
      (render [:post "/edit/new-post"] post))
    (resp/redirect "/login")))

(defpage [:post "/edit/save-page"] {:as post}
  (new-content! post :normal))

(defpage [:post "/edit/save-question"] {:as post}
  (new-content! post :question))

(defpage [:post "/edit/remove/:pid"] {:keys [pid conf undo] :as data}
  (let [id (obj-id pid)
        post (fetch-one :posts :where {:_id id})]
    (if (and post (current-id)
             (or (admin? (current-id)) (= (current-id) (:author post))))
      (if undo
        (remove-button post)
        (if conf
          (do (update! :posts {:_id id}
                       {:$set {:removed true :removed-by (current-id)}})
            "Rimosso")
          (remove-button post :confirm)))
      (resp/redirect (str "post/" pid)))))

(defpage [:post "/edit/comment/:pid"] {:keys [pid comment]}
  (let [post (fetch-one :posts :where {:_id (obj-id pid)})]
    (when (and (current-id) post (not (str/blank? comment)))
      (update! :posts {:_id (obj-id pid)}
        {:$push {:comments {:body (escape-html comment) :author (current-id)
                            :created-at (java.util.Date.)}}})
      (when (not (= (:author post) (current-id)))
        (update! :people {:_id (:author post)} ;; Update dell'autore
          {:$push {:news {:action :new-comment :post (:_id post)
                          :title (:title post) :time (java.util.Date.)}}}))))
  (post-comments (fetch-one :posts :where {:_id (obj-id pid)})))

(defpartial post-reply-table [question & [reply]]
  (form-to {:accept-charset "utf-8" } [:post (str (user-reply-path question) "/preview")]
    (error-table "Errore invio post")
    [:div.post
     [:table.post
      [:tr
       (let [p (fetch-one :people :where {:_id (current-id)})]
         [:td.postAuthor (user-description p)])]
      [:tr
       [:td.postDate (format-timestamp-relative (java.util.Date.))]]
      [:tr.postContent
       [:td.postContent {:colspan 2}
        (text-area {:class :ckeditor :rows 15 :placeholder "Contenuto del post"}
          :content (:content reply))]]
      [:tr.postBottom
       [:td.postActions {:colspan 2}
        (submit-button {:class "postReply"} "Anteprima e invio")]]]])
  new-post-help)

(defpage [:post "/edit/reply/:qid"] {:keys [qid] :as reply}
  (binding [*custom-header* ckeditor-header]
    (let [qid (obj-id qid)
          question (fetch-one :posts :where {:_id qid})
          ch (fetch-one :channels :where {:_id (:channel question)})]
      (layout "Rispondi"
        [:h1.channelName "Risposta a: " (:title question)]
        (if (and question (not (:removed question)))
          [:span
           (post-reply-table question reply)
           (post-div question)
           (post-answers question)]
          [:p "Post non trovato"])))))

(defpage "/edit/reply/:qid" {:keys [qid] :as reply}
  (render [:post "/edit/reply/:qid"] reply))

(defn valid-reply? [{:keys [qid title content type]}]
  (vali/rule (not (str/blank? content))
    [:content "Contenuto non valido"])
  (vali/rule (fetch-one :posts :where {:_id (obj-id qid)})
    [:post "Non esiste un post di domanda"])
  (not (vali/errors? :title :content)))

(defpage [:post "/edit/reply/:qid/preview"] {:keys [qid] :as reply}
  (if (and (current-id) (valid-reply? reply))
    (let [hiddens (html
                    (for [[name value] reply]
                      [:input {:type :hidden :name name :value value}])
                    [:input {:type :hidden :name :qid :value qid}])]
      (layout "Anteprima risposta"
        (post-div (merge reply {:author (current-id) :channel (:channel-id reply)
                                :type   "answer" :created-at (java.util.Date.)})
          :preview true)
        (form-to {:accept-charset "utf-8"} [:post (str "/edit/reply/" qid)]
          hiddens
          (submit-button {:class "postSubmit"} "Modifica risposta"))
        (form-to {:accept-charset "utf-8"} [:post (str "/edit/reply/" qid "/save")]
          hiddens
          (submit-button {:class "postSubmit"} "Invia risposta"))))
    (render [:post "/edit/reply/:qid"] reply)))
  
(defpage [:post "/edit/reply/:qid/save"] {:keys [qid] :as reply}
  (if (and (current-id) (valid-reply? reply))
    (let [qid (obj-id qid)
          question (fetch-one :posts :where {:_id qid})
          new-post (insert! :posts
                     {:title (str "Risposta a: " (:title question))
                      :content    (:content reply)
                      :author     (current-id)      :channel    (:channel question)
                      :created-at (java.util.Date.) :answers-to qid
                      :type       :answer
                      :keywords   (get-keywords (extract-html-text (:content reply)))})]
      (update! :channels {:_id (:channel question)} {:$inc {:posts 1}})
      (update! :posts {:_id qid} {:$inc {:answers 1}})
      (when (not (= (:author question) (current-id)))
        (update! :people {:_id (:author question)} ;; Update dell'autore
          {:$push {:news {:action :new-answer :post (:_id new-post)
                          :time (java.util.Date.)}}}))
      (resp/redirect (post-path question)))
    (render [:post "/edit/reply/:qid"] reply)))

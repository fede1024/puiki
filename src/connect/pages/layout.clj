(ns connect.pages.layout
  (:use connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require connect.errors
           connect.logs
           [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]
   [noir.util.test :as test]))

(def sh-header
  (html
    [:script {:type "text/javascript" :src "/syntaxhighlighter/scripts/shCore.js"}]
    [:script {:type "text/javascript" :src "/syntaxhighlighter/scripts/shAutoloader.js"}]
    [:link {:type "text/css" :rel "stylesheet" :href "/syntaxhighlighter/styles/shCoreDefault.css"}]))

(def sh-highlight
  (html
    [:script {:type "text/javascript" :src "/sh-highlight.js"}]))

(def google-analytics
  (html
    [:script {:type "text/javascript"}
     "  var _gaq = _gaq || [];
        _gaq.push(['_setAccount', 'UA-28804569-1']);
        _gaq.push(['_trackPageview']);

        (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
        })();"]))

(def fb-like-box
  (html
    [:iframe
     {:src "//www.facebook.com/plugins/likebox.php?href=http%3A%2F%2Fwww.facebook.com%2Fpages%2FPoliConnect%2F182526581833051&amp;width=180&amp;colorscheme=light&amp;show_faces=false&amp;border_color&amp;stream=false&amp;header=false&amp;height=62&amp;appId=220725871327778"
      :scrolling "no" :frameborder "0" :style "border:none; overflow:hidden; width:180px; height:62px;"
      :allowTransparency "true"}]))

(defpartial status-section []
  (let [recents (fetch-count :sessions :where {:last-access {:$gt (connect.logs/get-time-ago :minutes 8)}})]
    (if (current-id)
      (let [id (current-id)
            person (fetch-one :people :where {:_id id})]
        [:div.status
         [:table.status
          [:tr
           [:td.statusWelcome
            "Benvenuto " (:firstname person) "!"]]
          [:tr
           [:td.statusInfo "Matricola " id " " (link-to "/logout" "logout")]]
          [:tr
           [:td.statusOnline "Utenti online " (if (zero? recents) 1 recents)]]]])
      [:div.status
       [:p.login "Effettua il " 
        [:a {:href "/login" :id :loginLink} "login"]
        ;[:script "document.write('<a href = \"/login?redirect=' + document.URL + '\">login</a>')"]
        [:script "$('#loginLink').attr('href', '/login?redirect=' + document.URL);"]
        " oppure " [:a {:href "/register"} "registrati"] "."]
       [:div.center "Utenti online " (if (zero? recents) 1 recents)]])))

(defpartial people-table [people & {:keys [link img field edit id info date lastname]}]
  [:table.people
   (for [person people]
     (let [name (str (when lastname (str (:lastname person) " "))
                     (:firstname person))]
       (html
         [:tr.person
          [:td.personName
           (when img [:img {:src "/images/one-user.png"}]) " "
           (if link
             [:a {:href (user-info-path (:_id person))} name]
             name)
           (when (and edit (or (admin? (current-id)) (= (:_id person) (current-id))))
             [:span " - " (link-to (user-edit-path (:_id person)) "Modifica")])]
          (when date [:td.personDate (format-timestamp-relative (:created-at person))])]
         (when field [:tr [:td.personField (:field person)]])
         (when (or info id)
           [:tr 
            [:td.personId {:colspan "2"}
             (when info (str (translate-job (:job person)) " "))
             (when id (:_id person))]]))))])

;(defpartial last-posts []
;  [:h2.section "Ultimi post"]
;  [:table.lastPosts
;   (for [post (fetch :posts :sort {:created-at -1} :limit 5)]
;     [:tr.lastPost [:td.lastPostTitle (link-to (post-path post) (:title post))]
;      [:td.lastPostDate (format-timestamp-relative (:created-at post))]])])

(defpartial last-registrations []
  [:h2.section "Ultimi utenti registrati:"]
  (people-table (fetch :people :limit 5 :sort {:created-at -1})
     :field true :img true :link (not (nil? (current-id)))) [:br]
  "Totale " (link-to "/user/list" (fetch-count :people) " utenti") ".")

(defpartial user-sidebar []
  ;(last-posts)
   [:div.sideBarSection (last-registrations)])

(defpartial admin-sidebar []
  [:div.sideBarSection
   [:h2.section "Strumenti di amministrazione"]
   [:p (link-to "/admin/" "Pagina amministratore")]
   [:p (link-to "/logs/errors/" "Errori") " " (connect.errors/errors-count)]
   [:p (link-to "/admin/logs" "Logs")]
   [:p (link-to "/admin/feedbacks" "Feedbacks") " " (fetch-count :feedbacks)]])

(defpartial default-sidebar []
  (let [id (current-id)]
    (if (not id)
      [:div.sideBarSection (last-registrations)]
      (html
        (when (admin? id) (admin-sidebar))
        (when (user? id)  (user-sidebar))))))

(def ^:dynamic *sidebar* default-sidebar)
(def ^:dynamic *custom-header* nil)
(def ^:dynamic *like-button* nil)

(defpartial like-button [url]
  (let [complete-url (str "http://www.puiki.it" url)]
    [:iframe {:src (encode-url "https://www.facebook.com/plugins/like.php"
                               {:href complete-url :show_faces :false
                                :layout :button_count :ref :fb_like})
              :scrolling :no :frameborder 0
              :style "vertical-align: middle; border:none; width:95px; height:21px"}]))

(defpartial layout-header []
  [:div.headerContainer
   [:div.header
    [:a.logo {:href "/"}
     [:img.logo {:src "/images/puiki.png"}]]
    (when *like-button* (like-button "www.puiki.it"))
    [:div.rfloat
     [:a {:href "/" :title "Home"}
     [:img.header {:src "/images/home.png"}]]
     [:a {:href "/user/following" :title "Canali seguiti e notifiche"}
      (if (current-id)
        (let [news (count (:news (fetch-one :people :where {:_id (current-id)})))]
          (if (<= news 5)
            [:img.header.border {:src (str "/images/stars/" news ".png")}]
            [:img.header.border {:src "/images/stars/5more.png"}]))
        [:img.header.border {:src "/images/stars/0.png"}])]
     [:a {:href "/channel/list" :title "Tutti i canali"}
      [:img.header.border {:src "/images/channels.png"}]]
     [:a {:href "/search" :title "Ricerca"}
      [:img.header.border {:src "/images/search.png"}]]
     [:a {:href "/user/" :title "Info utente"}
      [:img.header.border {:src "/images/user.png"}]]
     (if (admin? (current-id))
       [:a {:href "/admin/" :title "Amministratore"}
        [:img.header.border {:src "/images/admin.png"}]]
       [:a {:href "/user/feedback" :title "Feedback"}
      [:img.header.border {:src "/images/feedback.png"}]])
     [:script {:type "text/javascript"}
   "$('img.header').hover(
       function() { $(this).css({ 'background-color': '#c2ddfd' })},
       function() { $(this).css({ 'background-color': '#AAD0FF' })}
);"]]]])

(defpartial layout-body [content]
  [:div.body
   [:div.sideBar
    (status-section)
    (if (fn? *sidebar*)
      (*sidebar*)
      (str *sidebar*))
     fb-like-box]
   [:div.content content sh-highlight]])

(defpartial layout-footer []
  [:div.footer
   "Powered by: "
   (link-to "http://www.clojure.org" [:img.footer {:height 25 :src "/images/Clojure.png"}])
   (link-to "http://www.mongodb.org" [:img.footer {:height 25 :src "/images/mongodb.png"}])
   (link-to "http://aws.amazon.com/" [:img.footer {:height 25 :src "/images/aws.png"}])])

(defpartial layout [title & content]
  (html5
    [:head
     [:meta {:charset "utf-8"}]
     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/> "
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
     [:meta {:property "og:image" :content "/images/logo-small.png"}]
     [:link {:rel "shortcut icon" :href "/images/favicon.ico"}]
     [:script {:type "text/javascript" :src "/jquery-1.7.1.min.js"}]
     ;[:script {:type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js"}]
     ;[:script {:type "text/javascript" :src "/test.js"}]
     (include-css "/css/reset.css")
     [:link {:href "http://fonts.googleapis.com/css?family=Ubuntu:400,500" :rel "stylesheet" :type "text/css"}]
     (include-css "/css/screen.css")
     (include-css "/css/search.css")
     (include-css "/css/people.css")
     (include-css "/css/channel.css")
     (include-css "/css/admin.css")
     (include-css "/css/post.css")
     sh-header
     *custom-header*
     [:title title]
     google-analytics]
    [:body
     [:span.loader {:id "loader" :style "display: none;"}
      " Caricamento..." [:img.loader {:src "/images/loading.gif"}]]
     (layout-header)
     (layout-body content)
     (layout-footer)]))
    
(defpartial error-text [errors]
  (map #(html [:p.errorMsg [:img.errorMsg {:src "/images/error.png"}] " " %]) errors))

(defn error-cell [field]
  [:td.errorMsg (when-let [err (first (vali/get-errors field))]
                (html [:img.errorMsg {:src "/images/error.png"}] " " err))])

(defn error-table [title]
  (when (not (empty? @vali/*errors*))
    [:div.error
     [:table.error
      [:tr.errorTitle [:td.errorTitle title]]
      (for [[field errors] @vali/*errors*]
        (for [error errors]
          [:tr.errorBody
           [:td.errorBody error]]))]]))

(defpage "/permission-denied" []
  (layout "Accesso negato"
    [:div
     [:h3 "Non hai permessi sufficienti per vedere questa pagina."]
     [:p (link-to "/" "Home")]]))

(defpage "/not-found" []
  (layout "Pagina inesistente"
    [:h2.section "Indirizzo non valido"]
    [:img.notFound {:src "/images/dead-end.png"}]
    "La pagina cercata non è stata trovata."))

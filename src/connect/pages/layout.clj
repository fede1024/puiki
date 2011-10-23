(ns connect.pages.layout
  (:use connect.pages.utils
        noir.core   
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers
        somnium.congomongo)
 (:require [noir.server :as server]
           [noir.validation :as vali]
           [noir.session :as session]
           [noir.response :as resp]
   [noir.util.test :as test]))

(defpartial status-section []
  (if (current-id)
    (let [id (current-id)
          person (fetch-one :people :where {:_id id})]
      [:table.status
       [:tr.statusWelcome
        [:td.statusInfo "Loggato come " id " ("
         (translate-job (:job person)) ") "]
        [:td.statusLogout (link-to "/logout" "Logout")]]])
    [:div.register "Effettua il " 
     [:a {:href "/login" :id :loginLink} "login"]
     ;[:script "document.write('<a href = \"/login?redirect=' + document.URL + '\">login</a>')"]
     [:script "$('#loginLink').attr('href', '/login?redirect=' + document.URL);"]
     " oppure " [:a {:href "/register"} "registrati"] "."]))

(defpartial people-table [people & {:keys [img id info date lastname]
                                    :or {img true}}]
  [:table.people
   (for [{matr :_id name :firstname lname :lastname
          job :job d :created-at} people]
     [:span
      [:tr.person
       [:td.personName (when img [:img {:src "/images/user-small.png" :height 13}]) " "
        name (when lastname (str " " lname))
        (when (or (admin? (current-id)) (= matr (current-id)))
          [:span " - " (link-to (user-edit-path matr) "Modifica")])]
      (when date [:td.personDate (format-timestamp-relative d)])]
      (when info [:tr [:td.personId {:colspan "2"} (translate-job job) " "
                       (when id matr)]])])])

(defpartial last-posts []
  [:h2.section "Ultimi post"]
  [:table.lastPosts
   (for [post (fetch :posts :sort {:created-at -1} :limit 5)]
     [:tr.lastPost [:td.lastPostTitle (link-to (post-path post) (:title post))]
      ;[:td.lastPostDate (format-timestamp-relative (:created-at post))]
      ])])

(defpartial last-registrations []
  [:h2.section "Ultimi utenti registrati:"]
  (people-table (fetch :people :limit 5 :sort {:created-at -1})
    :img true :date true :info true :lastname (current-id)))

(defpartial user-sidebar []
  (last-posts) (last-registrations))

(defpartial admin-sidebar []
  [:div.adminSidebar
   [:h2.section "Strumenti di amministrazione"]
   [:p (link-to "/admin/" "Pagina amministratore")]
   [:p (link-to "/logs/errors/" "Log degli errori.")]])

(defpartial default-sidebar []
  (let [id (current-id)]
    (if (not id)
      (html (last-posts) (last-registrations))
      (html
        (when (admin? id) (admin-sidebar))
        (when (user? id)  (user-sidebar))))))

(def sh-header
  (html
    [:script {:type "text/javascript" :src "/syntaxhighlighter/scripts/shCore.js"}]
    [:script {:type "text/javascript" :src "/syntaxhighlighter/scripts/shAutoloader.js"}]
    [:link {:type "text/css" :rel "stylesheet" :href "/syntaxhighlighter/styles/shCoreDefault.css"}]
    [:script {:type "text/javascript"}
     "SyntaxHighlighter.all();"]))

(def sh-autoloader
  (html
    [:script {:type "text/javascript" :src "/sh-autoloader.js"}]))

(def *sidebar* default-sidebar)
(def *custom-header* nil)

(defpartial layout [title & content]
  (html5
    [:head
     [:meta {:charset "utf-8"}]
     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/> "
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
     [:link {:rel "shortcut icon" :href "/images/favicon.gif"}]
     [:script {:type "text/javascript" :src "/jquery-1.6.4.min.js"}]
     (include-css "/css/reset.css")
     (include-css "/css/screen.css")
     (include-css "/css/search.css")
     (include-css "/css/people.css")
     (include-css "/css/channel.css")
     (include-css "/css/admin.css")
     (include-css "/css/post.css")
     sh-header
     *custom-header*
     [:title title]]
    [:body
     [:span.loader {:id "loader" :style "display: none;"}
        " Caricamento..." [:img.loader {:src "/images/loading.gif"}]]
     [:table.home
      [:tr.header
       [:td.header [:img.logo {:src "/images/polito.png"}] "PoliConnect"]
       [:td.links
        [:a.header {:href "/" :title "Home"}
         [:img.header {:src "/images/home.png"}]]
        [:a.header {:href "/search" :title "Ricerca"}
         [:img.header {:src "/images/search.png"}]]
        [:a.header {:href "/user/" :title "Info utente"}
         [:img.header {:src "/images/user.png"}]]
        [:a.header {:href "/user/following" :title "Canali seguiti e notifiche"}
         (if (current-id)
           (let [news (count (:news (fetch-one :people :where {:_id (current-id)})))]
             (if (<= news 5)
               [:img.header {:src (str "/images/stars/" news ".png")}]
               [:img.header {:src "/images/stars/5more.png"}]))
           [:img.header {:src "/images/stars/0.png"}])]
        [:a.header {:href "/admin/" :title "Amministratore"}
         [:img.header {:src "/images/admin.png"}]]]
       [:td.status (status-section)]]
      [:tr.sideBar
       [:td.content {:colspan 2} content]
       [:td.sideBar (if (fn? *sidebar*)
                      (*sidebar*)
                      (str *sidebar*))]]
      [:tr.footer
       [:td.footer {:colspan 3}
        "Powered by: "
        (link-to "http://www.clojure.org" [:img.footer {:height 25 :src "/images/Clojure.png"}])
        (link-to "http://www.mongodb.org" [:img.footer {:height 25 :src "/images/mongodb.png"}])
        (link-to "http://aws.amazon.com/" [:img.footer {:height 25 :src "/images/aws.png"}])]]]
     sh-autoloader]))
    
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

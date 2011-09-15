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
        [:td.statusWelcome (str "Benvenuto " (:firstname person) "! ")]
        [:td.statusLogout {:rowspan 2} (link-to "/logout" "Logout")]]
       [:tr.statusInfo
        [:td.statusInfo "Loggato come " id " ("
         (translate-job (:job person)) ") "]]])
    [:div.register "Effettua il " [:a {:href "/login"} "login"]
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
      (when date [:td.personDate (format-timestamp d)])]
      (when info [:tr [:td.personId {:colspan "2"} (translate-job job) " "
                       (when id matr)]])])])

(defpartial user-sidebar []
  [:div.userSidebar
   [:h2.section "Ultimi post"]
   [:table.lastPosts
    (for [post (fetch :posts :sort {:created-at -1} :limit 5)]
      [:tr.lastPost [:td.lastPostTitle (:title post)]
       [:td.lastPostDate (format-timestamp (:created-at post))]])]])

(defpartial admin-sidebar []
  [:div.adminSidebar
   [:h2.section "Strumenti di amministrazione"]
   [:p (link-to "/admin/" "Pagina amministratore")]
   [:p (link-to "/logs/errors/" "Log degli errori.")]])

(defpartial default-sidebar []
  (let [id (current-id)]
    (if (not id)
      [:span [:h2.section "Ultimi utenti registrati:"]
       (people-table (fetch :people :limit 5 :sort {:created-at -1})
         :img true :date true :info true)]
      [:span
       (when (user? id)  (user-sidebar))
       (when (admin? id) (admin-sidebar))])))

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
     (include-css "/css/reset.css")
     (include-css "/css/screen.css")
     (include-css "/css/search.css")
     (include-css "/css/people.css")
     (include-css "/css/channel.css")
     (include-css "/css/post.css")
     *custom-header*
     [:title title]]
    [:body
     [:table.home
      [:tr.header
       [:td.header title]
       [:td.links
        [:a.header {:href "/"} [:img.header {:src "/images/home.png"}]]
        [:a.header {:href "/search"} [:img.header {:src "/images/search.png"}]]
        [:a.header {:href "/user/"} [:img.header {:src "/images/user.png"}]]
        [:a.header {:href "/user/following"} [:img.header {:src "/images/asterisk-green.png"}]]
        [:a.header {:href "/admin/"} [:img.header {:src "/images/admin.png"}]]]
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
        (link-to "http://www.rackspace.com" [:img.footer {:height 25 :src "/images/Rackspace.png"}])]]]]))
    
(defpartial error-text [errors]
  (map #(html [:p [:img {:src "/images/error.png"}] " " %]) errors))

(defn error-cell [field]
  [:td.errImg (when-let [err (first (vali/get-errors field))]
                [:span [:img {:src "/images/error.png"}] " " err])])

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

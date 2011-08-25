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

(defpartial people-table [people & {:keys [img info date lastname]
                                    :or {img true}}]
  [:table.people
   (for [{id :_id name :firstname lname :lastname
          job :job d :created-at} people]
     [:span
      [:tr.person
       [:td.personName (when img [:img {:src "/images/user-small.png" :height 13}]) " "
        name (when lastname (str " " lname))
        (when (or (admin? (current-id)) (= id (current-id)))
          [:span " - " (link-to (user-edit-path id) "Modifica")])]
      (when date [:td.personDate (format-timestamp d)])]
      (when info [:tr [:td.personId {:colspan "2"} (translate-job job) " " id]])])])

(defpartial user-sidebar []
  [:div.userSidebar
   [:h2.userSidebarTitle "Ultimi post"]
   "TODO"
   [:h2 "Nuovo post"]
   (form-to [:get "/edit/new-post"]
     (submit-button {:class "postNew"} "Nuovo post (TODO: fix)"))])

(defpartial admin-sidebar []
  [:div.adminSidebar
   [:h2.adminSidebarTitle "Strumenti di amministrazione"]
   [:p (link-to "/admin/" "Pagina amministratore")]
   [:p (link-to "/logs/errors/" "Log degli errori.")]])

(defpartial default-sidebar []
  (let [id (current-id)]
    (if (not id)
      [:span [:h2.peopleTableTitle "Ultimi utenti registrati:"]
       (people-table (fetch :people :limit 5 :sort {:created-at -1})
         :img true :date true :info true)]
      [:span
       (when (user? id)  (user-sidebar))
       (when (admin? id) (admin-sidebar))])))

(def *sidebar* default-sidebar)

(defpartial layout [title & content]
  (html
    (doctype :html4)
    [:head
     [:meta {:charset "utf-8"}]
     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/> "
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
     [:link {:rel "shortcut icon" :href "/images/favicon.gif"}]
     (include-css "/css/reset.css")
     (include-css "/css/screen.css")
     ;(include-css "/css/comments.css")
     (include-css "/css/people.css")
     (include-css "/css/channel.css")
     (include-css "/css/post.css")
     (include-css "/css/sandbar-forms.css")
     [:title title]]
    [:body
     [:table.home
      [:tr.left
       [:td.header title]
       [:td.links
        [:a.header {:href "/"} [:img.header {:src "/images/home.png"}]]
        [:a.header {:href "/user/"} [:img.header {:src "/images/user.png"}]]
        [:a.header {:href "/user/following"} [:img.header {:src "/images/asterisk-green.png"}]]
        [:a.header {:href "/admin/"} [:img.header {:src "/images/admin.png"}]]]
       [:td.status (status-section)]]
      [:tr.right
       [:td.content {:colspan 2} content]
       [:td.sideBar (if (fn? *sidebar*)
                      (*sidebar*)
                      (str *sidebar*))]]
      [:tr.footer
       [:td.footer {:colspan 3}
        "Powered by: "
        (link-to "http://www.clojure.org" [:img.footer {:height 25 :src "/images/Clojure.png"}])]]]]))
    
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
    [:h2 "Indirizzo non valido"]
    [:img.notFound {:src "/images/dead-end.png"}]
    "La pagina cercata non è stata trovata."))

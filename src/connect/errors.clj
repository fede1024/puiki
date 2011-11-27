(ns connect.errors
 (:use [hiccup core page-helpers]
   noir.core))

(def *render-error* true)
(def *stacktrace-length* 20)

(def *log-errors* true)
(def *errors-dir* "resources/public/logs/errors/")
;(def *errors-dir* "/home/federico/connect/resources/public/logs/errors/")
(def *max-error-files* 20)

(defn exception-causes [#^Throwable t]
  (lazy-seq
     (cons t (when-let [cause (.getCause t)]
                 (exception-causes cause)))))

(defn stack-element-data [s]
  {:class (.getClassName s)
   :method (.getMethodName s)
   :file (.getFileName s)
   :line (.getLineNumber s)
   :str (.toString s)})

(defn exception-data [e]
  {:class (.getName (.getClass e))
   :message (.getMessage e)
   :stacktrace (map stack-element-data
                (.getStackTrace e))
   :str (.toString e)})

(defpartial stacktrace-table [st]
  [:table.stackTrace
   [:tr.stackHead
    [:td.stackHead "Classe:"]
    [:td.stackHead "Metodo:"]
    [:td.stackHead "File"]
    [:td.stackHead "Linea:"]]
   (for [elem st]
     [:tr.stackElem
      [:td.stackElemClass  (escape-html (:class elem))]
      [:td.stackElemMethod (escape-html (:method elem))]
      [:td.stackElemFile   (escape-html (:file elem))]
      [:td.stackElemLine   (escape-html (:line elem))]])])

(def *description*
  (str "Si è verificato un errore. La descrizione dell'errore è stata memorizzata "
    "e verrà investigata al più presto dagli amministratori."))

(defpartial error-section [e]
  (let [err (exception-data e)]
    [:div.exception
     [:table.exception
      [:tr.exception
       [:td.excHead "Classe errore:"]
       [:td.excClass (escape-html (:class err))]
       ] ;; Se admin: link agli errori
      [:tr.exception
       [:td.excHead "Messaggio errore:"]
       [:td.excMessage (escape-html (:message err))]]]
     [:h2.stackTraceTitle "Stacktrace:"]
     (stacktrace-table (take *stacktrace-length* (:stacktrace err)))]))

(defn error-page [title e & [request]]
  (html
    (doctype :html4)
    [:head
     [:meta {:charset "utf-8"}]
     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/> "
     (include-css "/css/reset.css")
     (include-css "/css/error.css")
     [:title title]]
    [:body
     [:div.content
      [:h1.title title]
      [:p.description *description*]
      [:span (for [cause (reverse (exception-causes e))]
               [:span 
                [:h2.error "Errore:"]
                (error-section cause)])]
      (when request
        [:span
         [:h2.error "Richiesta:"]
         [:p.request (escape-html request)]])]]))

(defn- format-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd_HH:mm:ss")
    (java.util.Date.)))

(defn- remove-old-errors []
  (let [files (sort-by #(.getName %)
                (.listFiles (java.io.File. *errors-dir*)))]
    (map #(.delete %) (take (- (count files) *max-error-files*) files))))

(defn remove-all-errors []
  (doall (map #(.delete %) (.listFiles (java.io.File. *errors-dir*)))))

(defn- store-page [html]
  (.mkdir (java.io.File. "resources/public/logs/")) ;; TODO: fix qui
  (.mkdir (java.io.File. *errors-dir*))
  (loop [n 0]
    (let [path (str *errors-dir* (format-date)
                 (if (> n 0) (str "(" n ")" ) ""))]
      (if (not (.exists (java.io.File. (str path ".html"))))
        (spit (str path ".html") html)
        (recur (inc n))))))

(defn log-error-page [title e & [request]]
  (let [html (error-page title e request)]
    (when *log-errors*
      (store-page html)
      (remove-old-errors))
    html))

;(defmacro check [title & content]
;  `(try
;     ~@content
;     (catch Exception exc#
;       (if *render-error*
;         (error-page ~title exc#)
;         (throw exc#)))))

(defmacro check [title & content]
  `(try
     ~@content
     (catch Throwable exc#
       (if *render-error*
         {:status 500 :headers {"Content-Type" "text/html"}
          :body (log-error-page ~title exc#)}
         (throw exc#)))))

(defn wrap-error-check [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable ex
        {:status 500 :headers {"Content-Type" "text/html"}
         :body (log-error-page (str "Errore pagina: " (:uri request)
                                  " (" (:request-method request)  ")")
                  ex request)}))))

;(defmacro GETx [path args description & body]
;  `(GET ~path ~args 
;     (err/check (str "Errore pagina: " ~description)
;       ~@body)))
;
;(defmacro POSTx [path args description & body]
;  `(POST ~path ~args 
;     (err/check (str "Errore pagina: " ~description)
;       ~@body)))

;(defn exception-causes [#^Throwable t]
;  (lazy-seq
;    (cons t (when-let [cause (.getCause t)]
;              (exception-causes cause)))))
;
;(take 5
;  (exception-causes (Exception. "no")))

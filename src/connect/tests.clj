(ns connect.tests
  (:require [clj-http.client :as client]))

;(def cook (:cookies (client/get "https://didattica.polito.it/pls/portal30/sviluppo.public_iscritti.init")))

;(def links
; (re-seq #"sviluppo.public_iscritti.espandi\?[^\"]*"
;   (:body
;     (client/post "https://didattica.polito.it/pls/portal30/sviluppo.public_iscritti.cerca"
;       {:form-params {:from_portlet "N" :a_acc "2012" :doc "scuderi" :cod_ins "" :nome_ins "" :p_header ""}}))))

(defn get-courses-links [anno docente]
  ;(println "\n" (java.util.Date.))
  (print "Cerco:" anno (pr-str docente))
  (let [links (re-seq  #"sviluppo.public_iscritti.espandi\?[^\"]*" ; #"sviluppo.public_iscritti.espandi\?([^\"]*)"
                (:body
                  (client/post "https://didattica.polito.it/pls/portal30/sviluppo.public_iscritti.cerca"
                    {:form-params {:from_portlet "N" :a_acc anno :doc docente :cod_ins "" :nome_ins "" :p_header ""}})))]
    (println " trovati" (count links) "corsi")
    links))

;(def links (get-courses-links 2012 "scuderi"))

;(def pagina (client/get (str "https://didattica.polito.it/pls/portal30/" (first links))))

;(def pagina (client/get "https://didattica.polito.it/pls/portal30/sviluppo.public_iscritti.espandi?p_a_acc=2012&p_cod_ins=23ACIOA%2023ACINZ%2023ACIPC&p_id_inc=56386&p_id_ins=87347&p_id_comm_esa=&p_alfa=GI-ZZ&p_header=&p_periodo=1"))

;(def pagina (client/get "https://didattica.polito.it/pls/portal30/sviluppo.public_iscritti.espandi?p_a_acc=2012&p_cod_ins=23ACIOA%2023ACINZ%2023ACIPC&p_id_inc=56386&p_id_ins=87347&p_id_comm_esa=&p_alfa=GI-ZZ&p_header=&p_periodo=1"))

(defn get-link-names [link]
  (map (fn [[_ matr cogn nome codinscred cds pd pi freq]]
       {:code matr :firstname nome :lastname cogn
        :codinscred codinscred :cds cds :pd pd :pi pi :freq freq})
     (re-seq #"<tr>
<td valign=\"top\"><font class=\"policorpo\">([0-9][0-9][0-9][0-9]+)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>
<td valign=\"top\"><font class=\"policorpo\">(.*)</font></td>"
               (:body (client/get (str "https://didattica.polito.it/pls/portal30/" link))))))

;(get-link-names (first links))

;(def data (doall (mapcat get-link-names links)))

;(def data
;  (reduce #(assoc % (:matr %2) %2) {}
;    (mapcat get-link-names links)))

(defn get-student-data [anno docente]
  (let [links (get-courses-links anno docente)]
    (reduce #(assoc % (:code %2) %2) {}
            (mapcat get-link-names links))))

;(def data (get-student-data 2012 "scuderi"))

(defn write-to-file [path data]
  (binding [*out* (new java.io.FileWriter path)]
    (dorun
      (map
        (fn [[m d]]
          (println (format "%s   %-20s   %s" m (:nome d) (:cogn d))))
        (sort-by first data)))))

;(write-to-file "/home/federico/scuderi"
;   (get-student-data 2012 "laf"))

(def alfabeto "abcdefghijklmnopqrstuvwxyz")

(defn gen-strings []
  (for [i alfabeto
        j alfabeto
        k alfabeto]
    (str i j k)))

;(binding [*out* (new java.io.FileWriter "/home/federico/log")]
;  (def data
;    (reduce merge (map #(get-student-data 2012 %)
;                       '("laface" "scuderi" "bonani")))))

;(binding [*out* (new java.io.FileWriter "/home/federico/log")]
;  (def data
;    (reduce merge (map #(do (get-student-data 2012 %)
;                          (Thread/sleep 1000))
;                       (gen-strings)))))

;(write-to-file "/home/federico/studenti" tmp)

(defn write-data-file [path data]
  (binding [*out* (new java.io.FileWriter path)]
    (println "{")
    (dorun
      (map
        (fn [[m d]]
          (println (format "\"%s\" %-20s" m (pr-str d))))
        (sort-by first data)))
    (println "}")))

;(take 10 (drop 56 seqs))

;(spit "/home/federico/db-seqs" (seq seqs))

;(spit "/home/federico/data" tmp)

;; 3382 + 18968 = 22350

;(def seqs
;  (filter #(> (second %) 0)
;    (map (fn [line]
;           (let [[_ n val] (re-find #"Cerco: 2012 \"([a-z][a-z][a-z])\" trovati ([0-9]*) " line)]
;             [n (Integer/parseInt val)]))
;         (line-seq (new java.io.BufferedReader (new java.io.FileReader "/home/federico/log"))))))

;(def vet (atom []))

;(println @vet)

;(dorun
;  (for [group @vet]
;    (dorun (for [[m d] group]
;             (println (format "%s   %-20s   %s" m (:nome d) (:cogn d)))))))

;(write-data-file "/home/federico/studenti2" (reduce merge @vet))

;(def matr-min (apply min (map #(Integer/parseInt (first %)) tmp)))
;(def matr-max (apply max (map #(Integer/parseInt (first %)) tmp)))

;(get tmp "166733")

;(dorun (for [[m d] ]
;         (println (format "%s   %-20s   %s" m (:nome d) (:cogn d)))))

;(get tmp "189048")

;(count (filter #(= (:nome (second %)) "ALBERTO") tmp))

;(count (reduce merge @vet))

;(def db-seq (map first (reverse (sort-by second seqs))))

;(take 1 (reverse db-seq))

;(binding [*out* (new java.io.FileWriter "/home/federico/log3")]
;  (def data
;    (reduce merge (map #(let [d (get-student-data 2012 %)]
;                          (swap! vet conj d)
;                          (Thread/sleep 1000) d)
;                       (drop 13 db-seq)))))

;(def data
;  (reduce merge
;    (for [[m d] (read-string (slurp "/home/federico/studenti2"))]
;      (let [cdsd (:cds d)
;            cds (if (some #(= \( %) cdsd)
;                  (second (re-find #"\((.*)\)" cdsd))
;                  cdsd)]
;        {m {:code (:code d) :firstname (:firstname d) :lastname (:lastname d) :cds cds}}))))

;(def data2 (read-string (slurp "/home/federico/Archivio/db-studenti")))

;(write-data-file "/home/federico/test2" (merge data2 data))

;(def data3 (merge data2 data))

;(take 2 data3)

;(dorun
;  (map #(println (:lastname (second %)) (:firstname (second %)))
;  (filter #(= (:cds (second %)) "FIS1T3")
;    data3)))

;(count
;  (filter #(= (:cds (second %)) nil)
;    data3))

;(map #(println (pr-str %)) (re-seq #"<a([^>]+)>(.+?)</a>" body))

;(re-seq #"sviluppo.public_iscritti.espandi\?[^\"]*" body)

;(client/post "http://localhost:1234"
;  {:form-params {:from_portlet "N" :a_acc "2012" :doc "scuderi" :cod_ins "" :nome_ins "" :p_header ""}
;   :cookies cook})
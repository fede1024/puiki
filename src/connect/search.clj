(ns connect.search
 (:use somnium.congomongo)
 (:require
   [clojure.contrib.combinatorics :as comb]))

(def stop-words
  #{"ad", "al", "allo", "ai", "agli", "all", "agl",
    "alla", "alle", "con", "col", "coi", "da", "dal", "dallo", "dai", "dagli", "dall",
    "dagl", "dalla", "dalle", "di", "del", "dello", "dei", "degli", "dell", "degl",
    "della", "delle", "in", "nel", "nello", "nei", "negli", "nell", "negl", "nella",
    "nelle", "su", "sul", "sullo", "sui", "sugli", "sull", "sugl", "sulla", "sulle",
    "per", "tra", "contro", "io", "tu", "lui", "lei", "noi", "voi", "loro", "mio",
    "mia", "miei", "mie", "tuo", "tua", "tuoi", "tue", "suo", "sua", "suoi", "sue",
    "nostro", "nostra", "nostri", "nostre", "vostro", "vostra", "vostri", "vostre",
    "mi", "ti", "ci", "vi", "lo", "la", "li", "le", "gli", "ne", "il", "un", "uno",
    "una", "ma", "ed", "se", "perché", "anche", "come", "dov", "dove", "che", "chi",
    "cui", "non", "più", "quale", "quanto", "quanti", "quanta", "quante", "quello",
    "quelli", "quella", "quelle", "questo", "questi", "questa", "queste", "si",
    "tutto", "tutti", "a", "c", "e", "i", "l", "o", "ho", "hai", "ha", "abbiamo",
    "avete", "hanno", "abbia", "abbiate", "abbiano", "avrò", "avrai", "avrà", "avremo",
    "avrete", "avranno", "avrei", "avresti", "avrebbe", "avremmo", "avreste",
    "avrebbero", "avevo", "avevi", "aveva", "avevamo", "avevate", "avevano", "ebbi",
    "avesti", "ebbe", "avemmo", "aveste", "ebbero", "avessi", "avesse", "avessimo",
    "avessero", "avendo", "avuto", "avuta", "avuti", "avute", "sono", "sei", "è",
    "siamo", "siete", "sia", "siate", "siano", "sarò", "sarai", "sarà", "saremo",
    "sarete", "saranno", "sarei", "saresti", "sarebbe", "saremmo", "sareste",
    "sarebbero", "ero", "eri", "era", "eravamo", "eravate", "erano", "fui", "fosti",
    "fu", "fummo", "foste", "furono", "fossi", "fosse", "fossimo", "fossero",
    "essendo", "faccio", "fai", "facciamo", "fanno", "faccia", "facciate", "facciano",
    "farò", "farai", "farà", "faremo", "farete", "faranno", "farei", "faresti",
    "farebbe", "faremmo", "fareste", "farebbero", "facevo", "facevi", "faceva",
    "facevamo", "facevate", "facevano", "feci", "facesti", "fece", "facemmo",
    "faceste", "fecero", "facessi", "facesse", "facessimo", "facessero", "facendo",
    "sto", "stai", "sta", "stiamo", "stanno", "stia", "stiate", "stiano", "starò",
    "starai", "starà", "staremo", "starete", "staranno", "starei", "staresti",
    "starebbe", "staremmo", "stareste", "starebbero", "stavo", "stavi", "stava",
    "stavamo", "stavate", "stavano", "stetti", "stesti", "stette", "stemmo", "steste",
    "stettero", "stessi", "stesse", "stessimo", "stessero", "stando" })

(defn tokenize [text]
  (re-seq #"[a-zA-Z0-9àèéìòù@]+" text))

(defn get-keywords [text]
  (map #(apply str (replace {\é \è} %))
    (filter #(not (stop-words %))
      (tokenize (.toLowerCase text)))))

(defn post-keywords [post]
  (distinct
    (concat (get-keywords (:title post))
      (get-keywords (:content post)))))

(defn- create-all-posts-keywords! []
  (for [p (fetch :posts)]
    (update! :posts {:_id (:_id p)}
      {:$set {:keywords (post-keywords p)}})))

(defn search-all-keywords [keywords & [channel]]
  (fetch :posts :where
    (merge {:keywords {:$all keywords}}
      (when channel {:channel channel}))))

(defn search-keyword [keyword & [channel]]
  (fetch :posts :where 
    (merge {:keywords keyword}
      (when channel {:channel channel}))))

(defn search-some-keywords [keywords & [channel]]
  (let [groups (group-by :_id
                 (mapcat #(fetch :posts :where
                            (merge {:keywords %}
                              (when channel {:channel channel})))
                   keywords))]
    (sort-by :relevance >
      (reverse ;; TODO: migliorare qui??
        (sort-by :created-at 
          (for [[id posts] groups]
            (assoc (first posts) :relevance (count posts))))))))

(defn search-by-text [text & [channel]]
  (search-some-keywords (get-keywords text) channel))


;(map :channel (search-some-keywords ["risposta"]))
;
;(map :channel (search-some-keywords ["risposta"]
;                (:_id (fetch-one :channels))))
;
;(map (fn [post] [(:relevance post) (:title post)])
;  (search-some-keywords ["risposta" "post" "primo"]))
;
;(map (fn [post] [(:relevance post) (:title post)])
;  (search-by-text "Risposta al primo Post"
;    (:_id (fetch-one :channels))))

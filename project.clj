(defproject connect "0.1"
  :description "PoliConnect"
  :main connect.core
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [noir "1.2.2"]
                 [congomongo "0.1.7"]
                 [commons-lang "2.5"]
                 [javax.mail/mail "1.4.3"]
                 [clj-http "0.2.2"]
                 [clj-json "0.4.3"]
                 [commons-codec "1.5"]
                 [net.java.dev.jets3t/jets3t "0.8.1"]]
  :dev-dependencies [[com.github.robertrolandorg/lein-eclipse "1.1.0"]]
  :repl-retry-limit 2000
  :keep-non-project-classes true)


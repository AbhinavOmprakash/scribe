(defproject scribe "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :plugins [[lein-auto "0.1.3"]
            [lein-cloverage "1.0.9"]
            [lein-eftest "0.5.9"]]
  :repl-options {:init-ns scribe.core})

(defproject scribe "pre-release-SNAPSHOT"
  :description "Scribe is an ORM for postgres"
  :url "https://github.com/AbhinavOmprakash/scribe/blob/master/src/scribe/core.clj"
  :license {:name "MIT"
            :url "https://github.com/git/git-scm.com/blob/main/MIT-LICENSE.txt"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :plugins [[lein-auto "0.1.3"]
            [lein-cloverage "1.0.9"]
            [lein-eftest "0.5.9"]]
  :repl-options {:init-ns scribe.core})

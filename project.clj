(defproject site-diff-checker "0.1.0-SNAPSHOT"
  :description "This is a tool to check what pages are not transfered to the new site"
  :url "http://t.me/SanderK"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/core.async "0.4.474"]
                 [clj-http "3.7.0"]]
  :main site-diff-checker.core
  :bin {:name "site-diff"
          :bin-path "~/.local/bin"
          :bootclasspath false}
  :target-path "target"
  :profiles {:uberjar {:aot :all}})

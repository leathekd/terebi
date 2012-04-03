(defproject terebi "1.0.0"
  :description "A simple Clojure interface to thetvdb.com's API"
  :dependencies [[clj-http "0.3.2"]
                 [org.clojure/clojure "1.3.0"]
                 [org.clojure/data.zip "0.1.0"]
                 [slingshot "0.10.2"]]
  :dev-dependencies [[clojure-source "1.3.0"]]
  :test-selectors {:remote :remote
                   :default (complement :remote)
                   :all (constantly true)})
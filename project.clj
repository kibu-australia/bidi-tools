(defproject kibu/bidi-tools "0.3.1"
  :description "Tools for bidi"
  :url "https://github.com/kibu-australia/bidi-tools"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.cemerick/url "0.1.1"]
                 [bidi "1.21.1" :scope "provided"]
                 [prismatic/schema "1.0.4"]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]])

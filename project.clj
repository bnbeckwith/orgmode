(defproject bnbeckwith/orgmode "0.7.5"
  :description "Org-mode parser"
  :url "http://github.com/bnbeckwith/orgmode"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [hiccup "1.0.5"]
                 [org.apache.commons/commons-lang3 "3.11"]]
  :profiles {:dev {:dependencies [[midje "1.9.9"]]}})

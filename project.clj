(defproject cst "0.1.0-SNAPSHOT"
  :description "Companion project to my blog post on Willet Time"
  :url "https://andrewstahlman.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter/incanter-core "1.5.7"]
                 [incanter/incanter-charts "1.5.7"]]
  :main ^:skip-aot willet-time.core
  :target-path "target/%s"
  :exclusions [org.clojure/tools.trace]
  :profiles {:uberjar {:aot :all}})

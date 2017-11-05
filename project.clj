(defproject willet-time "0.1.0-SNAPSHOT"
  :description "Companion project to my blog post on Willet Time"
  :url "https://andrewstahlman.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [incanter/incanter-core "1.5.7"]
                 [incanter/incanter-charts "1.5.7"]]
  :source-paths ["cljs.jar" "src/" "test/"]
  :main ^:skip-aot willet-time.core
  :target-path "target/%s"
  :exclusions [org.clojure/tools.trace]
  :plugins [[lein-figwheel "0.5.10"]
            [lein-cljsbuild "1.1.6"]]
  :cljsbuild {:builds [{:id "clock"
                        :source-paths ["src/"]
                        :figwheel true
                        :compiler {:main "willet-time.clock"
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js/out"}}
                       {:id "core"
                        :source-paths ["src/"]
                        :figwheel true
                        :compiler {:main "willet-time.core"
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/core.js"
                                   :output-dir "resources/public/js/out-core"}}]}
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[com.cemerick/piggieback "0.2.1"]
                                  [figwheel-sidecar "0.5.0-2"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})

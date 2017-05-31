(require 'cljs.build.api)

(cljs.build.api/build "src" {:main 'cst.core
                             :output-to "out/main.js"})

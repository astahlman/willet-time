(require 'cljs.build.api)

(cljs.build.api/watch "src"
  {:main 'cst.core
   :output-to "out/main.js"})

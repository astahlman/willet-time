(ns cst.core
  (:require [clojure.browser.repl :as repl]))

(comment (defonce conn
           (repl/connect "http://localhost:9000/repl")))

(enable-console-print!)

(println "Hello world!")

(comment (defn foo [a b]
           (* a b)))

(ns willet-time.data
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn load-dataset
  ([] (load-dataset "seattle-sunrise-sunset.csv"))
  ([csv-file]
   (letfn [(parse-str-as-hr [s]
            (let [hr (Integer/parseInt (subs s 0 2))
                  min (Integer/parseInt (subs s 2 4))]
              (+ hr (/ min 60.0))))]
    (for [[sunrise sunset]
          (rest ;; skip headers
           (map #(str/split (str/trim %) #",") (-> csv-file
                                                   (io/resource)
                                                   (slurp)
                                                   (str/split-lines))))]
      [(parse-str-as-hr sunrise) (parse-str-as-hr sunset)]))))

(ns willet-time.data
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn load-dataset
  ([]
   "Source: http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=2017&task=0&state=WA&place=seattle"
   (load-dataset "seattle-sunrise-sunset.csv"))
  ([csv-file]
   "Expected format: HHMM,HHMM. First column is sunrise, second is
   sunset. The first row is expected to be a header. The following
   rows each correspond to one calendar day, starting with January
   1st."
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

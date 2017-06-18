(ns willet-time.core-test
  (:require [clojure.test :refer :all]
            [willet-time.core :refer :all]
            [willet-time.data :as data]))

(def seattle-sunrises (map first (data/load-dataset)))

(deftest maximum-prediction-error []
  (testing "Our predicted time of sunrise is within 5 minutes of the
  time published by the US Naval Observatory"
    (let [max-error-minutes (apply max (error-minutes seattle-latitude seattle-sunrises))]
      (is (<= max-error-minutes 5)))))

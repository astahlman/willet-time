(ns willet-time.core-test
  (:require [clojure.test :refer :all]
            [willet-time.core :refer :all]))

(deftest maximum-prediction-error []
  (testing "Our predicted time of sunrise is within 5 minutes of the
  time published by the US Naval Observatory"
    (let [max-error-minutes (apply max (map error-minutes (range 0 364)))]
      (is (<= max-error-minutes 5)))))

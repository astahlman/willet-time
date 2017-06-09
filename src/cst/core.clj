(ns cst.core
  (import jodd.datetime.JDateTime)
  (:require [incanter core charts]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

;; Links:
;; 1. http://www.physics.rutgers.edu/~twatts/sunrise/node6.html
;; 2. http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=2017&task=0&state=WA&place=seattle
;; 3. http://totaleclipse.eu/Astronomy/EOT.html

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def ^:const seattle-latitude 47.38) ;; degrees N
;; TODO: This is significantly more accurate if you set the solstice to 356, corresponding to December 23rd
;; Why?
(def ^:const winter-solstice-doy 354) ;; solstice was on December 21, in 2016 (0-based index)
(def ^:const alpha 23.439) ;; degrees, measure of the ecliptic
(def ^:const hours-per-day 24.0)

(def ^:const cities
  {:seattle {:coords {:lat 47.6062 :lon -122.3321}
             :test-data {"2017-01-01" {:sunrise "2017-01-01T07:58:00-08:00"
                                       :sunset "2017-01-01T16:29:00-08:00"}
                         "2017-03-01" {:sunrise "2017-03-01T06:49:00-08:00"
                                       :sunset "2017-03-01T17:55:00-08:00"}
                         "2017-06-01" {:sunrise "2017-06-01T05:15:00-07:00"
                                       :sunset "2017-06-01T21:00:00-07:00"}
                         "2017-09-01" {:sunrise "2017-09-01T06:28:00-07:00"
                                       :sunset "2017-09-01T19:49:00-07:00"}
                         "2017-12-01" {:sunrise "2017-12-01T07:37:00-08:00"
                                       :sunset "2017-12-01T16:20:00-08:00"}}}})


(defn- close-enough?
  "Is delta between two Julian dates within 1 minute?"
  [a b]
  (println (.getTimeInMillis a))
  (println (.getTimeInMillis b))
  (let [millis-diff (Math/abs (- (.getTimeInMillis a)
                                 (.getTimeInMillis b)))]
    (println millis-diff)
    (<= millis-diff 60000)))

(defn- add-millis [jd millis]
  (let [d (.clone jd)]
    (.addMillisecond d millis)))

(defn- add-seconds [jd seconds]
  (add-millis jd (* 1000 seconds)))

(defn- iso->instant
  "Convert an ISO-8601 string to a java.time.Instant"
  [s]
  (java.time.Instant/from (.parse java.time.format.DateTimeFormatter/ISO_OFFSET_DATE_TIME s)))

(defn- instant->jdt
  "Convert a java.time.Instant to a JDateTime"
  [instant]
  (.changeTimeZone
   (JDateTime. (.toEpochMilli instant))
   (java.util.TimeZone/getTimeZone "UTC")))

(defn- jdt
  "Convert the given Julian Day (a numeric) to a JDateTime in the UTC time zone"
  [jd]
  (.setTimeZone
   (JDateTime. jd)
   (java.util.TimeZone/getTimeZone "UTC")))

(defn- iso->jdt
  "Convert an ISO-8601 string to a JDateTime"
  [s]
  (-> s
      (iso->instant)
      (instant->jdt)))

(assert every?
        (for [ts ["2017-05-27T11:59:00Z"
                  "2017-05-27T12:00:00Z"
                  "2017-05-27T12:01:00Z"]]
          (close-enough?
           (iso->jdt ts)
           (jdt 2457901.0))))

(let [now (JDateTime.)]
  (assert (not (close-enough? now (add-seconds now 61))))
  (assert (close-enough? now (add-seconds now 60)))
  (assert (close-enough? now now))
  (assert (close-enough? now (add-seconds now -60)))
  (assert (not (close-enough? now (add-seconds now -61))))
  (assert (close-enough? (iso->jdt "2017-08-08T00:00:00Z")
                         (iso->jdt "2017-08-08T00:00:59Z")))
  (assert (close-enough? (iso->jdt "2017-08-08T00:00:00Z")
                         (iso->jdt "2017-08-08T00:01:00Z")))
  (assert (not (close-enough? (iso->jdt "2017-08-08T00:00:00Z")
                              (iso->jdt "2017-08-08T00:01:01Z")))))

(defn- radian->hr [r]
  (* 24
     (/ r (* Math/PI 2))))

(defn- psi
  "Angle of sun's highest elevation"
  [days-since-winter-solstice]
  (let [d (/ days-since-winter-solstice 365.25)
        Pi2d (* Math/PI 2 d)
        numerator (* (- 1 (Math/cos (Math/toRadians alpha)))
                     (Math/sin Pi2d)
                     (Math/cos Pi2d))
        denominator (+
                     (* (Math/cos (Math/toRadians alpha))
                        (Math/cos Pi2d)
                        (Math/cos Pi2d))
                     (* (Math/sin Pi2d)
                        (Math/sin Pi2d)))]
    (Math/atan (/ numerator denominator))))

(defn- sunrise-to-noon-angle-precise
  "Return the difference (in radians) between of the azimuth of P and
  at sunrise and the Sun's angle of highest elevation at solar noon."
  [latitude days-since-winter-solstice]
  (let [lambda (Math/toRadians latitude)
        d (/ days-since-winter-solstice 365.25)
        Pi2d (* Math/PI 2 d)
        numerator (* -1
                     (Math/cos Pi2d)
                     (Math/sin (Math/toRadians alpha))
                     (Math/cos (psi days-since-winter-solstice)))
        denominator (+
                     (* (Math/cos (Math/toRadians alpha))
                        (Math/cos Pi2d)
                        (Math/cos Pi2d))
                     (* (Math/sin Pi2d)
                        (Math/sin Pi2d)))
        tan-Beta (/ numerator denominator)
        tan-Lambda (Math/tan lambda)]
    (Math/acos (* -1
                  tan-Beta
                  tan-Lambda))))

(defn- length-of-morning
  "Return the number of hours from sunrise to noon at the given
  latitude and days since the winter solstice"
  [latitude days-since-winter-solstice]
  (radian->hr
   (sunrise-to-noon-angle-precise
    latitude
    days-since-winter-solstice)))

(defn- fmt-hours [n]
  (let [minutes (* 60.0 (- n (Math/floor n)))]
    (format "%02d:%02d" (int (Math/floor n)) (int minutes))))

(assert (= "01:00" (fmt-hours 1)))
(assert (= "01:15" (fmt-hours 1.25)))
(assert (= "07:30" (fmt-hours 7.50)))

(defn- eqn-of-time-eccentricity
  "Drift from solar noon due to the eccentricity of the Earth's
  orbit. We've been assuming Earth's orbit is circular, so we bolt
  this on at the end to make our calculation a little more
  accurate. Amplitude taken from http://totaleclipse.eu/Astronomy/EOT.html"
  [days-since-winter-solstice]
  (let [d (/ days-since-winter-solstice 365.25)
        Pi2d (* Math/PI 2 d)
        amplitude-minutes 7.66]
    (* (/ amplitude-minutes 60) (Math/sin Pi2d))))

(defn- solar-noon-drift
  "Return the difference in hours between solar noon and mean solar
  noon on day [0 - 365.25), where 0 corresponds to the winter
  solstice"
  [days-since-winter-solstice]
  (let [eccentricity-component (eqn-of-time-eccentricity days-since-winter-solstice)
        ecliptic-component (radian->hr (psi days-since-winter-solstice))]
    (+ eccentricity-component ecliptic-component)))

(defn sunrise
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (- (+ 12.0 (solar-noon-drift days-since-winter-solstice))
     (length-of-morning latitude days-since-winter-solstice)))

(defn- load-seattle-dataset []
  (letfn [(parse-str-as-hr [s]
            (let [hr (Integer/parseInt (subs s 0 2))
                  min (Integer/parseInt (subs s 2 4))]
              (+ hr (/ min 60.0))))]
    (for [[sunrise sunset]
          (rest ;; skip headers
           (map #(str/split (str/trim %) #",") (-> "seattle-sunrise-sunset.csv"
                                                   (io/resource)
                                                   (slurp)
                                                   (str/split-lines))))]
      [(parse-str-as-hr sunrise) (parse-str-as-hr sunset)])))

(defn- nth-sunrise-time [n]
  (first (nth (load-seattle-dataset) n)))

(defn- days-since-winter-solstice
  "The number of days since December 21st (the date of the December
  solstice in 2016) for the given day of the calendar year, where 0
  corresponds to January 1st and 364 corresponds to December
  31st (we're ignoring leap years)."
  [doy]
  (if (< doy winter-solstice-doy)
    (+ (- 365 winter-solstice-doy) doy)
    (- doy winter-solstice-doy)))

(assert (= 11 (days-since-winter-solstice 0))) ;; January 1st is 11 days after the solstice
(assert (= 364 (days-since-winter-solstice (dec winter-solstice-doy))))
(assert (zero? (days-since-winter-solstice winter-solstice-doy)))
(assert (= 1 (days-since-winter-solstice (inc winter-solstice-doy))))
(assert (= 10 (days-since-winter-solstice 364)))

(defn error-minutes
  "Error between predicted and actual sunrise in minutes"
  [doy]
  (let [actual (nth-sunrise-time doy)
        predicted (sunrise seattle-latitude
                           (days-since-winter-solstice doy))
        delta (- actual predicted)]
    (* 60 delta)))

(defn- plot-components []
  (incanter.core/view (-> (incanter.charts/function-plot (partial length-of-morning seattle-latitude) 0 364)
                          (incanter.charts/set-title "Length of morning")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Hours")))

  (incanter.core/view (-> (incanter.charts/function-plot #(* 60 (solar-noon-drift %)) 0 364)
                          (incanter.charts/set-title "Drift from mean solar noon")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Minutes"))))


(defn- plot-sunrise-predictions []
  (incanter.core/view (-> (incanter.charts/function-plot nth-sunrise-time 0 364)
                          (incanter.charts/add-function (fn [doy]
                                                          (sunrise
                                                           seattle-latitude
                                                           (days-since-winter-solstice doy))) 0 364)
                          (incanter.charts/set-title "Time of Sunrise")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Hours")))


  (incanter.core/view (-> (incanter.charts/function-plot error-minutes 0 364)
                          (incanter.charts/set-title "Error")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Minutes"))))

(plot-sunrise-predictions)
(assert (<=
         (apply max (map error-minutes (range 0 364)))
         5)
        "Our predicted time of sunrise is within 5 minutes of the time
        published by the US Navy")


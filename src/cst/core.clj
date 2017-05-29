(ns cst.core
  (import jodd.datetime.JDateTime)
  (:require [incanter core charts]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

;; Links:
;; 1. http://www.physics.rutgers.edu/~twatts/sunrise/node6.html
;; 2. http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=2017&task=0&state=WA&place=seattle

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


;; (defn- daylight-interval
;;   "Returns a 2-item sequence with items sunrise and sunset, both of type
;;   jdt"
;;   [{:keys [lat lon]} doy-str]
;;   [nil nil])

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
  (* r (/ 24 (* Math/PI 2))))

(defn- psi
  "Time of sun's highest elevation"
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
    (/ numerator denominator)))

(defn- sunrise-to-noon-angle-precise
  "Return the angle (in radians) of ___?"
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
  (let [lambda (Math/toRadians latitude)
        d (/ days-since-winter-solstice 365.25)
        sunrise-to-noon-angle (sunrise-to-noon-angle-precise latitude days-since-winter-solstice)]
    (radian->hr sunrise-to-noon-angle)))

(defn- fmt-hours [n]
  (let [minutes (* 60.0 (- n (Math/floor n)))]
    (format "%02d:%02d" (int (Math/floor n)) (int minutes))))

(assert (= "01:00" (fmt-hours 1)))
(assert (= "01:15" (fmt-hours 1.25)))
(assert (= "07:30" (fmt-hours 7.50)))

(defn- solar-noon-drift
  "Return the difference in hours between solar noon and mean solar
  noon on day [0 - 365.25), where 0 corresponds to the winter
  solstice"
  [days-since-winter-solstice]
  (radian->hr (Math/atan (psi days-since-winter-solstice))))

(defn sunrise
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (- (+ 12.0 (solar-noon-drift days-since-winter-solstice))
     (length-of-morning latitude days-since-winter-solstice)))

;; (incanter.core/view (-> (incanter.charts/function-plot (partial sunrise seattle-latitude) 0 364)
;;                         (incanter.charts/set-title "Sunrise")
;;                         (incanter.charts/set-x-label "Day of year")
;;                         (incanter.charts/set-y-label "Hour")))

(comment (defn run-test []
           (for [[name {:keys [coords test-data]}] cities]
             (for [[date {:keys [sunrise sunset]}] test-data]
               (assert (and
                        (close-enough? (iso->jdt sunrise) (:sunrise (daylight-interval coords date)))
                        (close-enough? (iso->jdt sunset) (:sunset (daylight-interval coords date)))))))))

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
  "For some reason there's a systematic bias that disappears if you
  shift the curve by 2 days. Until I figure out what's going on, I'm
  using a cheap hack by pretending the solstice is on Dec. 23 instead
  Dec. 21"
  ([doy] (days-since-winter-solstice doy true))
  ([doy use-bias?]
   (let [maybe-biased-solstice (if use-bias?
                                 (+ 2 winter-solstice-doy)
                                 winter-solstice-doy)]
     (if (< doy maybe-biased-solstice)
       (+ (- 365 maybe-biased-solstice) doy)
       (- doy maybe-biased-solstice)))))

(assert (= 11 (days-since-winter-solstice 0 false)))
(assert (= 9 (days-since-winter-solstice 0 true)))
(assert (= 364 (days-since-winter-solstice 353 false)))
(assert (zero? (days-since-winter-solstice winter-solstice-doy false)))
(assert (= 1 (days-since-winter-solstice 355 false)))
(assert (= 10 (days-since-winter-solstice 364 false)))

(defn error-minutes
  "Error between predicted and actual sunrise in minutes"
  [doy]
  (let [actual (nth-sunrise-time doy)
        predicted (sunrise seattle-latitude
                           (days-since-winter-solstice doy))
        delta (- actual predicted)]
    (* 60 delta)))

(defn- plot-components []
  (incanter.core/view (-> (incanter.charts/function-plot (partial length-of-morning seattle-latitude) 0 365)
                          (incanter.charts/set-title "Length of morning")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Hours")))

  (incanter.core/view (-> (incanter.charts/function-plot #(* 60 (solar-noon-drift %)) 0 365)
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


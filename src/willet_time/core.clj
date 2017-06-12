(ns willet-time.core
  (import java.awt.Color)
  (:require [incanter core charts]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

;; Links:
;; 1. http://www.physics.rutgers.edu/~twatts/sunrise/node6.html
;; 2. http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=2017&task=0&state=WA&place=seattle
;; 3. http://totaleclipse.eu/Astronomy/EOT.html

(def ^:const seattle-latitude 47.38) ;; degrees N
(def ^:const winter-solstice-doy 354) ;; solstice was on December 21 in 2016 (0-based index)
(def ^:const alpha 23.439) ;; degrees, measure of the ecliptic
(def ^:const hours-per-day 24.0)
(def ^:const noon 12.0)

(defn- radian->hr [r]
  (* hours-per-day
     (/ r (* Math/PI 2))))

(defn- fmt-hours [n]
  (let [minutes (* 60.0 (- n (Math/floor n)))]
    (format "%02d:%02d" (int (Math/floor n)) (int minutes))))

(assert (= "01:00" (fmt-hours 1)))
(assert (= "01:15" (fmt-hours 1.25)))
(assert (= "07:30" (fmt-hours 7.50)))

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

(declare daylight-hours)

(defn sunrise
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (first (daylight-hours latitude days-since-winter-solstice)))

(defn sunset
  "Return the hour of sunset at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (second (daylight-hours latitude days-since-winter-solstice)))

(defn- daylight-hours
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (let [solar-noon (+ noon (solar-noon-drift days-since-winter-solstice))
        length-of-day (* 2 (length-of-morning latitude days-since-winter-solstice))]
    [(- solar-noon (/ length-of-day 2))
     (+ solar-noon (/ length-of-day 2))]))

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
  (-> (incanter.charts/function-plot nth-sunrise-time 0 364)
      (incanter.charts/add-function (fn [doy]
                                      (sunrise
                                       seattle-latitude
                                       (days-since-winter-solstice doy))) 0 364)
      (incanter.charts/set-title "Time of Sunrise")
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Hours")))


(defn- plot-sunrise-predictions-error []
  (-> (incanter.charts/function-plot error-minutes 0 364)
      (incanter.charts/set-title "Error")
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Minutes")))

(let [max-error-minutes (apply max (map error-minutes (range 0 364)))]
  (assert (<= max-error-minutes 5)
          "Our predicted time of sunrise is within 5 minutes of the
          time published by the US Navy"))

(defn- set-background-color [chart color]
  (do
    (.setBackgroundPaint (.getPlot chart) color)
    chart))

(defn- plot-daylight-hours [sunrise-hr sunset-hr & [title]]
  (-> (incanter.charts/xy-plot)
      (incanter.charts/set-x-range 0 364)
      (incanter.charts/set-y-range 3.5 22.5)
      (set-background-color java.awt.Color/BLUE)
      (incanter.charts/set-background-alpha 0.65)
      (incanter.charts/set-alpha 0.8)
      (incanter.charts/add-polygon
       (concat sunrise-hr (reverse sunset-hr))
       :fill-paint java.awt.Color/YELLOW)
      (incanter.charts/set-title (or title "Daylight Hours"))
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Hour of Day")))

(defn plot-standard-daylight-hours []
  (let [domain (range 0 365)
        sunrise-hr (for [x domain]
                     [x (sunrise seattle-latitude
                                 (days-since-winter-solstice x))])
        sunset-hr (for [x domain]
                    [x (sunset seattle-latitude
                               (days-since-winter-solstice x))])]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Standard)")))

(defn plot-willet-time-daylight-hours
  "Plot the daylight hours under Willet Time where the earliest
  sunrise is at hour sunrise-floor"
  [sunrise-floor]
  (let [domain (range 0 365)
        sunrise-hr-original (for [x domain]
                              [x (sunrise seattle-latitude (days-since-winter-solstice x))])
        sunset-hr-original (for [x domain]
                             [x (sunset seattle-latitude (days-since-winter-solstice x))])
        offset (map (fn [[x hr]]
                      (max 0 (- sunrise-floor hr)))
                    sunrise-hr-original)
        sunrise-hr (map (fn [[x hr] offset] [x (+ hr offset)]) sunrise-hr-original offset)
        sunset-hr (map (fn [[x hr] offset] [x (+ hr offset)]) sunset-hr-original offset)]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Willet Time)")))

(defn plot-dst-daylight-hours
  "Plot the daylight hours under Daylight Saving Time in 2017 the United States (DST in effect from March 12th - November 5th)"
  []
  (let [domain (range 0 365)
        [dst-start-doy dst-end-doy] [71 309]
        in-dst? #(and (>= % dst-start-doy)
                      (<= % dst-end-doy))
        offset #(if (in-dst? %) 1 0)
        sunrise-hr (for [x domain]
                     [x (+ (offset x)
                           (sunrise seattle-latitude
                                    (days-since-winter-solstice x)))])
        sunset-hr (for [x domain]
                    [x (+ (offset x)
                          (sunset seattle-latitude
                                  (days-since-winter-solstice x)))])]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Daylight Saving Time)")))

(defn -main
  "Print graphs comparing daylight hours under Standard, Daylight
  Saving, and Willet Time"
  [& args]
  (incanter.core/save
   (plot-standard-daylight-hours)
   "assets/standard-daylight-hours.png")
  (incanter.core/save
   (plot-dst-daylight-hours)
   "assets/dst-daylight-hours.png")
  (incanter.core/save
   (plot-willet-time-daylight-hours 5.5)
   "assets/willet-time-daylight-hours.png")
  (incanter.core/save
   (plot-sunrise-predictions)
   "assets/sunrise-predictions.png")
  (incanter.core/save
   (plot-sunrise-predictions-error)
   "assets/sunrise-predictions-error.png"))

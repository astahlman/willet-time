(ns willet-time.core
  (:require
   [clojure.string :as str]))

;; References:
;; 1. http://www.physics.rutgers.edu/~twatts/sunrise/node6.html
;; 2. http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=2017&task=0&state=WA&place=seattle
;; 3. http://totaleclipse.eu/Astronomy/EOT.html

(def ^:const seattle-latitude 47.38) ;; degrees N
(def ^:const winter-solstice-doy 354) ;; solstice was on December 21 in 2016 (0-based index)
(def ^:const ALPHA 23.439) ;; degrees, measure of the ecliptic
(def ^:const hours-per-day 24.0)
(def ^:const noon 12.0)
(def ^:const PI 3.14159265)

;; The sun rises no earlier than 05:30 under Willet Time
;; (This choice is totally arbitary.)
(def ^:const sunrise-floor 5.5)

(defn- radian->hr [r]
  (* hours-per-day
     (/ r (* PI 2))))

(defn sin
  "sin(x) where x is in radians"
  [x]
  #?(:clj (Math/sin x)
     :cljs (.sin js/Math x)))

(defn cos
  "cos(x) where x is in radians"
  [x]
  #?(:clj (Math/cos x)
     :cljs (.cos js/Math x)))

(defn acos
  "acos(x) where x is in radians"
  [x]
  #?(:clj (Math/acos x)
     :cljs (.acos js/Math x)))

(defn tan
  "tan(x) where x is in radians"
  [x]
  #?(:clj (Math/tan x)
     :cljs (.tan js/Math x)))

(defn atan
  "atan(x) where x is in radians"
  [x]
  #?(:clj (Math/atan x)
     :cljs (.atan js/Math x)))

(defn deg->radians
  "Convert x degrees to radians"
  [x]
  (/ (* x 2 PI)
     360.0))

(defn psi
  "Angle of sun's highest elevation"
  [days-since-winter-solstice]
  (let [d (/ days-since-winter-solstice 365.25)
        Pi2d (* PI 2 d)
        numerator (* (- 1 (cos (deg->radians ALPHA)))
                     (sin Pi2d)
                     (cos Pi2d))
        denominator (+
                     (* (cos (deg->radians ALPHA))
                        (cos Pi2d)
                        (cos Pi2d))
                     (* (sin Pi2d)
                        (sin Pi2d)))]
    (atan (/ numerator denominator))))

(defn sunrise-to-noon-angle-precise
  "Return the difference (in radians) between of the azimuth of P and
  at sunrise and the Sun's angle of highest elevation at solar noon."
  [latitude days-since-winter-solstice]
  (let [lambda (deg->radians latitude)
        d (/ days-since-winter-solstice 365.25)
        Pi2d (* PI 2 d)
        numerator (* -1
                     (cos Pi2d)
                     (sin (deg->radians ALPHA))
                     (cos (psi days-since-winter-solstice)))
        denominator (+
                     (* (cos (deg->radians ALPHA))
                        (cos Pi2d)
                        (cos Pi2d))
                     (* (sin Pi2d)
                        (sin Pi2d)))
        tan-Beta (/ numerator denominator)
        tan-Lambda (tan lambda)]
    (let [x (* -1
             tan-Beta
             tan-Lambda)]
      (acos x))))

(defn length-of-morning
  "Return the number of hours from sunrise to noon at the given
  latitude and days since the winter solstice"
  [latitude days-since-winter-solstice]
  (radian->hr
   (sunrise-to-noon-angle-precise
    latitude
    days-since-winter-solstice)))

(defn eqn-of-time-eccentricity
  "Drift from solar noon due to the eccentricity of the Earth's
  orbit. We've been assuming Earth's orbit is circular, so we bolt
  this on at the end to make our calculation a little more
  accurate. Amplitude taken from http://totaleclipse.eu/Astronomy/EOT.html"
  [days-since-winter-solstice]
  (let [d (/ days-since-winter-solstice 365.25)
        Pi2d (* PI 2 d)
        amplitude-minutes 7.66]
    (* (/ amplitude-minutes 60) (sin Pi2d))))

(defn solar-noon-drift
  "Return the difference in hours between solar noon and mean solar
  noon on day [0 - 365.25), where 0 corresponds to the winter
  solstice"
  [days-since-winter-solstice]
  (let [eccentricity-component (eqn-of-time-eccentricity days-since-winter-solstice)
        ecliptic-component (radian->hr (psi days-since-winter-solstice))]
    (+ eccentricity-component ecliptic-component)))

(declare daylight-hours days-since-winter-solstice)

(defn ^:export sunrise
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to Jan. 1"
  [latitude doy]
  (first (daylight-hours latitude (days-since-winter-solstice doy))))

(defn ^:export sunset
  "Return the hour of sunset at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds Jan. 1"
  [latitude doy]
  (second (daylight-hours latitude (days-since-winter-solstice doy))))

(defn- daylight-hours
  "Return the hour of sunrise at the given latitude (in degrees) on
  day [0 - 365.25), where 0 corresponds to the winter solstice"
  [latitude days-since-winter-solstice]
  (let [solar-noon (+ noon (solar-noon-drift days-since-winter-solstice))
        length-of-day (* 2 (length-of-morning latitude days-since-winter-solstice))]
    [(- solar-noon (/ length-of-day 2))
     (+ solar-noon (/ length-of-day 2))]))

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
  "Error between predicted and actual sunrise in minutes, given a day
  of year, latitude, and actual sunrise times"
  [lat actuals]
  (let [predictions (map #(sunrise lat %)
                         (range 0 365))
        residuals (map - actuals predictions)]
    (map #(* % 60) residuals)))

(defn offset-from-standard-time
  "Return the offset in hours from standard time"
  [latitude doy]
  (let [sunrise-hr (sunrise latitude doy)]
    (max 0 (- sunrise-floor sunrise-hr))))

(ns cst.core
  (import jodd.datetime.JDateTime)
  (:require [incanter core charts]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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

(def ^:const seattle-latitude 47.6062)

(defn daylight-interval
  "Returns a 2-item sequence with items sunrise and sunset, both of type
  jdt"
  [{:keys [lat lon]} doy-str]
  [nil nil])

(defn close-enough?
  "Is delta between two Julian dates within 1 minute?"
  [a b]
  (println (.getTimeInMillis a))
  (println (.getTimeInMillis b))
  (let [millis-diff (Math/abs (- (.getTimeInMillis a)
                                 (.getTimeInMillis b)))]
    (println millis-diff)
    (<= millis-diff 60000)))

(defn add-millis [jd millis]
  (let [d (.clone jd)]
    (.addMillisecond d millis)))

(defn add-seconds [jd seconds]
  (add-millis jd (* 1000 seconds)))

(defn iso->instant
  "Convert an ISO-8601 string to a java.time.Instant"
  [s]
  (java.time.Instant/from (.parse java.time.format.DateTimeFormatter/ISO_OFFSET_DATE_TIME s)))

(defn instant->jdt
  "Convert a java.time.Instant to a JDateTime"
  [instant]
  (.changeTimeZone
   (JDateTime. (.toEpochMilli instant))
   (java.util.TimeZone/getTimeZone "UTC")))

(defn jdt
  "Convert the given Julian Day (a numeric) to a JDateTime in the UTC time zone"
  [jd]
  (.setTimeZone
   (JDateTime. jd)
   (java.util.TimeZone/getTimeZone "UTC")))

(defn iso->jdt
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


(def ^:const alpha 23.439) ;; degrees, measure of the ecliptic
(def ^:const hours-per-day 24.0)

(defn radian->hr [r]
  (* r (/ 24 (* Math/PI 2))))

(defn length-of-morning [latitude doy]
  (let [lambda (Math/toRadians latitude)
        d (/ doy 365.25)
        sunrise-to-noon-angle_orig (- (/ Math/PI 2)
                                      (* (Math/toRadians alpha) (Math/tan lambda) (Math/cos (* 2 Math/PI d))))
        sunrise-to-noon-angle (sunrise-to-noon-angle-precise latitude doy)]

    (radian->hr sunrise-to-noon-angle)))

(defn sunrise-to-noon-angle-precise [latitude doy]
  (let [lambda (Math/toRadians latitude)
        d (/ doy 365.25)
        Pi2d (* Math/PI 2 d)
        numerator (* -1
                     (Math/cos Pi2d)
                     (Math/sin (Math/toRadians alpha))
                     (Math/cos (psi doy)))
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

(defn sunrise-to-noon-angle [latitude doy]
  (let [lambda (Math/toRadians latitude)
        d (/ doy 365.25)]
    (- (/ Math/PI 2)
       (* (Math/toRadians alpha) (Math/tan lambda) (Math/cos (* 2 Math/PI d))))))

(incanter.core/view (-> (incanter.charts/function-plot (partial length-of-morning seattle-latitude) 0 365)
                        (incanter.charts/set-title "Length of morning")
                        (incanter.charts/set-x-label "Day of year")
                        (incanter.charts/set-y-label "Hours")))
(defn fmt-hours [n]
  (let [minutes (* 60.0 (- n (Math/floor n)))]
    (format "%02d:%02d" (int (Math/floor n)) (int minutes))))

(assert (= "01:00" (fmt-hours 1)))
(assert (= "01:15" (fmt-hours 1.25)))
(assert (= "07:30" (fmt-hours 7.50)))

(fmt-hours (* 2 (length-of-morning seattle-latitude 0)))

(defn solar-noon-drift
  "Return the difference in hours between solar noon and mean solar
  noon for the given day of the year [0 - 365.25)"
  [doy]
  (radian->hr (Math/atan (psi doy))))

(defn sunrise
  [latitude doy]
  (- (+ 12.0 (solar-noon-drift doy))
     (length-of-morning latitude doy)))

;; Our more accurate calcution
(comment (incanter.core/view (-> (incanter.charts/function-plot #(* 60 (solar-noon-drift %)) 0 365)
                                 (incanter.charts/set-title "Drift from mean solar noon")
                                 (incanter.charts/set-x-label "Day of year")
                                 (incanter.charts/set-y-label "Minutes"))))

;; Their approximation
(comment (incanter.core/view (-> (incanter.charts/function-plot #(* (* 60 0.15) (Math/sin (* 4 Math/PI (/ % 365.25)))) 0 365)
                                 (incanter.charts/set-title "Drift from mean solar noon")
                                 (incanter.charts/set-x-label "Day of year")
                                 (incanter.charts/set-y-label "Minutes"))))


(incanter.core/view (-> (incanter.charts/function-plot (partial sunrise seattle-latitude) 0 364)
                        (incanter.charts/set-title "Sunrise")
                        (incanter.charts/set-x-label "Day of year")
                        (incanter.charts/set-y-label "Hour")))

(defn psi
  "Time of sun's highest elevation"
  [doy]
  (let [d (/ doy 365.25)
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

(radian->hr (psi 92))

(defn day-len2 [latitude doy]
  (let [lambda (Math/toRadians latitude)
        d (/ doy 365.25)
        Pi2d (* Math/PI 2 d)
        numerator (* -1
                     (Math/cos Pi2d)
                     (Math/sin (Math/toRadians alpha))
                     (Math/cos (psi doy)))
        denominator (+
                     (* (Math/cos (Math/toRadians alpha))
                        (Math/cos Pi2d)
                        (Math/cos Pi2d))
                     (* (Math/sin Pi2d)
                        (Math/sin Pi2d)))
        tan-Beta (/ numerator denominator)
        tan-Lambda (Math/tan lambda)]
    (* 2.0
       (radian->hr (Math/acos (* -1
                                 tan-Beta
                                 tan-Lambda))))))

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

(incanter.core/view (-> (incanter.charts/function-plot nth-sunrise-time 0 364)
                        (incanter.charts/add-function (partial sunrise seattle-latitude) 0 364)
                        (incanter.charts/set-title "Time of Sunrise")
                        (incanter.charts/set-x-label "Day of year")
                        (incanter.charts/set-y-label "Hours")))

(defn error-minutes
  "Error between predicted and actual sunrise in minutes"
  [doy]
  (let [actual (nth-sunrise-time doy)
        predicted (sunrise seattle-latitude doy)
        delta (- actual predicted)]
    (* 60 delta)))

(def ^:const winter-solstice )
(defn- align-on-solstice
  "The firt row in our test dataset corresponds to January 1st, but
  we're measuring doy from the winter solstice, so we need to shift
  it"
  []
  )

(incanter.core/view (-> (incanter.charts/function-plot error-minutes 0 364)
                        (incanter.charts/set-title "Error")
                        (incanter.charts/set-x-label "Day of year")
                        (incanter.charts/set-y-label "Minutes")))


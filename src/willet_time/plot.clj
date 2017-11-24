(ns willet-time.plot
  (import java.awt.Color)
  (:require [incanter core charts]
            [clojure.string :as str]
            [willet-time.core :as core]
            [willet-time.data :as data])
  (:gen-class))


(defn- plot-components []
  (incanter.core/view (-> (incanter.charts/function-plot (partial core/length-of-morning core/seattle-latitude) 0 364)
                          (incanter.charts/set-title "Length of morning")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Hours")))

  (incanter.core/view (-> (incanter.charts/function-plot #(* 60 (core/solar-noon-drift %)) 0 364)
                          (incanter.charts/set-title "Drift from mean solar noon")
                          (incanter.charts/set-x-label "Day of year")
                          (incanter.charts/set-y-label "Minutes"))))

(def actual-seattle-daylight-hours (data/load-dataset))
(def actual-seattle-sunrises (map first actual-seattle-daylight-hours))

(defn- plot-sunrise-predictions []
  (-> (incanter.charts/function-plot #(nth actual-seattle-sunrises %)
                                     0 364)
      (incanter.charts/add-function #(core/sunrise core/seattle-latitude %)
                                    0 364)
      (incanter.charts/set-title "Time of Sunrise")
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Hours")))

(defn- prediction-error-in-seattle-on [doy]
  "Error in minutes for the predicted time of sunrise in Seattle on
  the given day of the year"
  (nth
   (core/error-minutes
    core/seattle-latitude
    actual-seattle-sunrises)
   doy))

(defn- plot-sunrise-predictions-error []
  (-> (incanter.charts/function-plot prediction-error-in-seattle-on 0 364)
      (incanter.charts/set-title "Error")
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Minutes")))

(defn- plot-offset-from-standard-time [lat]
  (-> (incanter.charts/function-plot #(core/offset-from-standard-time lat %) 0 364)
      (incanter.charts/set-title "Offset from Standard Time")
      (incanter.charts/set-x-label "Day of year")
      (incanter.charts/set-y-label "Minutes")))

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
                     [x (core/sunrise core/seattle-latitude x)])
        sunset-hr (for [x domain]
                    [x (core/sunset core/seattle-latitude x)])]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Standard)")))

(defn plot-willet-time-daylight-hours [sunrise-floor]
  "Plot the daylight hours under Willet Time where the earliest
  sunrise is at hour sunrise-floor"
  (let [domain (range 0 365)
        sunrise-hr-original (for [x domain]
                              [x (core/sunrise core/seattle-latitude x)])
        sunset-hr-original (for [x domain]
                             [x (core/sunset core/seattle-latitude x)])
        offset (map (fn [[x hr]]
                      (max 0 (- sunrise-floor hr)))
                    sunrise-hr-original)
        sunrise-hr (map (fn [[x hr] offset] [x (+ hr offset)]) sunrise-hr-original offset)
        sunset-hr (map (fn [[x hr] offset] [x (+ hr offset)]) sunset-hr-original offset)]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Willet Time)")))

(defn plot-dst-daylight-hours []
  "Plot the daylight hours under Daylight Saving Time in 2017 the
  United States (DST in effect from March 12th - November 5th)"
  (let [march-12th 71 november-5th 309
        [dst-start-doy dst-end-doy] [march-12th november-5th]
        domain (range 0 365)
        in-dst? #(and (>= % dst-start-doy)
                      (<= % dst-end-doy))
        offset #(if (in-dst? %) 1 0)
        sunrise-hr (for [x domain]
                     [x (+ (offset x)
                           (core/sunrise core/seattle-latitude x))])
        sunset-hr (for [x domain]
                    [x (+ (offset x)
                          (core/sunset core/seattle-latitude x))])]
    (plot-daylight-hours sunrise-hr
                         sunset-hr
                         "Daylight Hours (Daylight Saving Time)")))

(defn- plot-sun-angle-of-highest-elevation []
  "The Sun's angle of highest elevation throughout the year"
  (incanter.core/view
   (-> (incanter.charts/function-plot core/psi 0 364)
       (incanter.charts/set-title "Angle of Sun's highest elevation")
       (incanter.charts/set-x-label "Days since winter solstice")
       (incanter.charts/set-y-label "Angle (radians)"))))

(defn- plot-sunrise-times-slices []
  (incanter.core/view
   (-> (incanter.charts/function-plot #(core/sunrise % 0) -90 90)
       (incanter.charts/add-function #(core/sunrise % 90) -90 90)
       (incanter.charts/add-function #(core/sunrise % 180) -90 90)
       (incanter.charts/add-function #(core/sunrise % 270) -90 90)
       (incanter.charts/set-title "Hour of sunrise")
       (incanter.charts/set-x-label "Latitude (deg)")
       (incanter.charts/set-y-label "Hour of sunrise"))))

(defn- plot-sunrise-times-shaded []
  "x: day of year
   y: latitude
   color: hour of sunrise"
  (incanter.core/view
   (->
    (let [f (fn [doy lat]
              (let [hr (core/sunrise lat doy)]
                (if (Double/isNaN hr)
                  -1.0 ;; No sunrise
                  hr)))]
      (incanter.charts/heat-map f 0 364 -90.0 90.0 :z-label "Hour"))
    (incanter.charts/set-x-label "Day of year")
    (incanter.charts/set-y-label "Latitude (degrees)")
    (incanter.charts/set-title "Hour of sunrise by latitude throughout the year"))))

(defn- plot-offset-from-standard-time-shaded []
  (incanter.core/view
   (->
    (let [sunrise-floor 6.0
          f (fn [doy lat]
              (let [hr (core/sunrise lat doy)]
                (if (Double/isNaN hr)
                  -1.0 ;; No sunrise
                  (max 0 (- sunrise-floor hr)))))]
      (incanter.charts/heat-map f 0 364 -90.0 90.0 :z-label "Hour")))))

(defn -main [& args]
  "Print graphs comparing daylight hours under Standard, Daylight
  Saving, and Willet Time"
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
   "assets/sunrise-predictions-error.png")
  (incanter.core/save
   (plot-offset-from-standard-time core/seattle-latitude)
   "assets/offset-from-standard-time.png"))


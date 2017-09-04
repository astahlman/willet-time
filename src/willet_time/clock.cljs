(ns willet-time.clock
  (:require [clojure.browser.repl :as repl]
            [willet-time.core :as core]
            [cljs.tools.reader :refer [read-string]]
            [goog.date :as gdate]
            [goog.date.DateTime :as gdatetime]
            [goog.date.Interval]))

(comment
  (defonce conn
    (repl/connect "http://localhost:9000/repl")))

(enable-console-print!)

(defrecord Clock [hr min sec])

(defn dom-el [id]
  (.getElementById js/document id))

(defn- now-seattle-pst
  "The current local time in Seattle (without daylight savings). We
  ignore the browser's local time zone and consider only the hour,
  minute, and second fields of the returned object."
  []
  (let [pst-offset-millis (* 1000 60 60 -8)
        local-time (gdate/DateTime.)
        utc-ms (+ (.getTime local-time)
                  (* 60000 (.getTimezoneOffset local-time)))]
    (gdatetime/fromTimestamp (+ utc-ms pst-offset-millis))))

(defn- now-doy
  "Current day of the year in Seattle"
  []
  (.getDayOfYear (now-seattle-pst)))

(defn- now-seattle-willet
  "The current Willet time in Seattle. Again, we ignore the timezone
  in the browser and care about only the hour, minute, and second
  fields"
  []
  (let [dt (now-seattle-pst)
        days-since-solstice (core/days-since-winter-solstice
                             (.getDayOfYear dt))
        offset-hrs (core/offset-from-standard-time
                    core/seattle-latitude
                    days-since-solstice)
        offset-seconds (* offset-hrs 60 60)]
    (do
      (.add dt
            (gdate/Interval.
             goog.date.Interval/SECONDS
             offset-seconds))
      dt)))

(defn- gdate->Clock [dt]
  (Clock. (.getHours dt)
          (.getMinutes dt)
          (.getSeconds dt)))

(declare get-clock-face-center)

(defn- rotate-element [el deg]
  (let [x (.getAttribute el "x")
        y (.getAttribute el "y")
        {:keys [x y]} (get-clock-face-center)]
    (.setAttribute el "transform" (str "rotate(" deg " " x " " y ")"))))

(def clocks
  {
  ;; {:willet {:now-fn #(gdate->Clock (now-seattle-willet))
  ;;           :hands-el-prefix "willet_"
  ;;           :text-el-id "willet_time_text"}
  ;;  :pst {:now-fn #(gdate->Clock (now-seattle-pst))
  ;;        :hands-el-prefix "pst_"
  ;;        :text-el-id "pst_text"}
   :grandfather {:now-fn #(gdate->Clock (now-seattle-willet))
                 :hands-el-prefix "grandfather_"
                 :text-el-id "grandfather_text"}})

(def ^:const img-original-width 2564)
(def ^:const img-original-height 3012)
(def svg-canvas-el (dom-el "willet_clock2"))

(defn get-clock-face-center
  ([] (get-clock-face-center
       (.getAttribute svg-canvas-el "width")
       (.getAttribute svg-canvas-el "height")))
  ([svg-width svg-height]
   (let [face-center-x 1320
         face-center-y 1028]
     {:x (* (/ face-center-x img-original-width) svg-width)
      :y (* (/ face-center-y img-original-height) svg-height)})))

(defn get-clock-hand-lengths
  ([] (get-clock-hand-lengths
       (.getAttribute svg-canvas-el "width")
       (.getAttribute svg-canvas-el "height")))
  ([svg-width svg-height]
   (let [original-clock-diameter 472
         original-clock-radius (/ original-clock-diameter 2)
         scaling-factor (Math/min
                         (/ svg-height img-original-height)
                         (/ svg-width img-original-width))]
     {:second (* .9 scaling-factor original-clock-radius)
      :minute (* scaling-factor original-clock-radius)
      :hour (* .6 scaling-factor original-clock-radius)})))

;; TODO: This only needs to happen once
(defn position-hands-at-center-of-face!
  [hr-hand-el min-hand-el sec-hand-el]
  (let [{face-mid-x :x
         face-mid-y :y} (get-clock-face-center)
        {second-length :second
         minute-length :minute
         hour-length :hour} (get-clock-hand-lengths)]

    (.setAttribute sec-hand-el "x1" face-mid-x)
    (.setAttribute sec-hand-el "x2" face-mid-x)
    (.setAttribute sec-hand-el "y1" (- face-mid-y second-length))
    (.setAttribute sec-hand-el "y2" face-mid-y)
    (.setAttribute sec-hand-el "height" second-length)

    (let [width (.getAttribute min-hand-el "width")
          ;; ratio of the arm length that actually sweeps around (i.e., subtract half of the dot in the middle)]
          effective-arm-length (* .96 minute-length)]
      (.setAttribute min-hand-el "x" (- face-mid-x (/ width 2)))
      (.setAttribute min-hand-el "y" (- face-mid-y effective-arm-length))
      (.setAttribute min-hand-el "height" minute-length))

    (let [width (.getAttribute hr-hand-el "width")
          ;; ratio of the arm length that actually sweeps around (i.e., subtract half of the dot in the middle)]
          effective-arm-length (* .81 hour-length)]
      (.setAttribute hr-hand-el "x" (- face-mid-x (/ width 2)))
      (.setAttribute hr-hand-el "y" (- face-mid-y effective-arm-length))
      (.setAttribute hr-hand-el "height" hour-length))))

(defn- draw-clock!
  [{:keys [now-fn hands-el-prefix]}]
  (let [{:keys [hr min sec]} (now-fn)
        sec-hand-el (dom-el (str hands-el-prefix "sec_hand"))
        min-hand-el (dom-el (str hands-el-prefix "min_hand"))
        hour-hand-el (dom-el (str hands-el-prefix "hour_hand"))]
    (do
      (position-hands-at-center-of-face! hour-hand-el min-hand-el sec-hand-el)
      (rotate-element sec-hand-el (* 6 sec))
      (rotate-element min-hand-el (* 6 min))
      (rotate-element hour-hand-el (+ (* 30 (mod hr 12))
                                      (/ min 2))))))
(defn- update-time-text!
  [{:keys [hr min sec]} el]
  (letfn [(fmt-num [n]
            (if (< n 10)
              (str "0" n)
              (str n)))]
    (aset el "innerHTML" (str (fmt-num hr) ":"
                              (fmt-num min) ":"
                              (fmt-num sec)))))

;; Set the canvas background

;; pendulum displacement (in degrees) is described by a sin wave
;; with period = 1 second and amplitude 30 (totally arbitrary)
;; we take t = 0 as the time of page load

(def t_0 (.getTime (gdate/DateTime.)))
(def ^:const original-pendulum-width 207)
(def ^:const original-pendulum-height 509)
(def ^:const original-pendulum-pivot-x 1320)
(def ^:const original-pendulum-pivot-y 1425)

;; TODO: Split placement from rotation, just do placement once
(defn rotate-pendulum!
  ([] (rotate-pendulum!
       (.getAttribute svg-canvas-el "width")
       (.getAttribute svg-canvas-el "height")))
  ([svg-width svg-height]
   (let [el (dom-el "pendulum")
         scaling-factor (Math/min
                         (/ svg-height img-original-height)
                         (/ svg-width img-original-width))
         pendulum-width (* scaling-factor original-pendulum-width)
         pendulum-height (* scaling-factor original-pendulum-height)
         pendulum-pivot-x (* scaling-factor original-pendulum-pivot-x)
         pendulum-pivot-y (* scaling-factor original-pendulum-pivot-y)
         x (- pendulum-pivot-x (/ pendulum-width 2.0))
         y pendulum-pivot-y
         t (- (.getTime (gdate/DateTime.)) t_0)
         freq 0.5
         amplitude 8
         displacement (* amplitude (Math/sin (/ (* 2 Math/PI t) 2000)))]
     (.setAttribute el "x" x)
     (.setAttribute el "y" y)
     (.setAttribute el "width" pendulum-width)
     (.setAttribute el "height" pendulum-height)
     (.setAttribute el "transform" (str "rotate(" displacement " " x " " y ")")))))

;; Draw the clock and update the text
(doseq [[name config] clocks]
  (js/setInterval #(draw-clock! config) 1000)
  (js/setInterval rotate-pendulum! 50)
  (let [tick (:now-fn config)
        el (dom-el (:text-el-id config))]
    (js/setInterval #(update-time-text! (tick) el) 1000)))

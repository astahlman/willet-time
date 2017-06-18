(ns willet-time.clock
  (:require [clojure.browser.repl :as repl]
            [willet-time.core :as core]
            [goog.date :as gdate]
            [goog.date.DateTime :as gdatetime]
            [goog.date.Interval]))

(comment
  (defonce conn
    (repl/connect "http://localhost:9000/repl")))

(enable-console-print!)

(defrecord Clock [hr min sec])

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

(defn- rotate-element [el deg]
  (.setAttribute el "transform" (str "rotate(" deg " 50 50)")))

(def clocks
  {:willet {:now-fn #(gdate->Clock (now-seattle-willet))
            :hands-el-prefix "willet_"
            :text-el-id "willet_time_text"}
   :pst {:now-fn #(gdate->Clock (now-seattle-pst))
         :hands-el-prefix "pst_"
         :text-el-id "pst_text"}})

(defn- draw-clock!
  [{:keys [now-fn hands-el-prefix]}]
  (let [{:keys [hr min sec]} (now-fn)
        sec-hand-el (js/eval (str hands-el-prefix "sec_hand"))
        min-hand-el (js/eval (str hands-el-prefix "min_hand"))
        hour-hand-el (js/eval (str hands-el-prefix "hour_hand"))]
    (do
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

;; Draw the clock and update the text
(doseq [[name config] clocks]
  (js/setInterval #(draw-clock! config) 1000)
  (let [tick (:now-fn config)
        el (js/eval (:text-el-id config))]
    (js/setInterval #(update-time-text! (tick) el) 1000)))

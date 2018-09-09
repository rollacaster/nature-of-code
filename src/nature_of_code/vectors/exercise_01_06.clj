(ns nature-of-code.vectors.exercise-01-06
  (:require [quil.core :as q]))

(defn add [v1 v2]
  (vector (+ (first v1) (first v2))
          (+ (second v1) (second v2))))

(def location (atom [250 250]))
(def velocity (atom [0 0]))
(def acceleration (atom [0 0]))
(def xoff (atom 0.0))
(def yoff (atom 0.0))

(defn setup []
  (q/background 255))

(defn draw []
  (q/clear)
  (q/background 255)
  (let [noise (q/map-range (q/noise @xoff @yoff) 0 1 -0.0001 0.0001)
        acceleration (swap! acceleration #(vector (+ (first %) noise)
                                                  (+ (second %) noise)))
        velocity (swap! velocity #(add % acceleration))
        location (swap! location #(add % velocity))]
    (q/fill 0)
    (q/ellipse (min (q/width) (max 0 (first location)))
               (min (q/height) (max 0 (second location))) 16 16))
  (swap! yoff #(+ 0.001 %))
  (swap! xoff #(+ 0.001 %)))

(q/defsketch perlin-acceleration
  :title "perlin-acceleration"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

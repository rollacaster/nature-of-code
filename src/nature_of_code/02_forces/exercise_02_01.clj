(ns nature-of-code.02-forces.exercise-02-01
  (:require [quil.core :as q]
            [nature-of-code.vector :as v]))

(def location (atom [250 250]))
(def velocity (atom [0 0]))
(def acceleration (atom [0 0]))

(def gravity [0 -0.05])

(def xoff (atom 0.001))

(defn setup []
  (q/background 255))

(defn draw []
  (q/clear)
  (q/background 255)
  (q/fill 255 0 0)
  (let [wind (vector (q/map-range (q/noise @xoff) 0 1 -0.05 0.05) 0)
        velocity (swap! velocity #(v/add wind (v/add gravity (v/add % @acceleration))))
        [x y] (swap! location #(let [[x y] (v/add % velocity)]
                                (vector
                                 (if (> x 500) 500 (if (< x 0) 0 x))
                                 (if (> y 500) 500 (if (< y 0) 0 y)))))]
    (swap! acceleration #(v/mult % 0))
    (q/ellipse x y 20 32))
  (when (<= (second @location) 0) (swap! velocity #(vector 0 (- (/ (second %) 2)))))
  (when (and (<= (second @velocity) 0.02)
             (= (second @location) 0))
    (reset! location [250 250])
    (reset! velocity [0 0]))
  (swap! xoff #(+ % 0.001)))

(defn run []
  (q/defsketch baloon
    :title "baloon"
    :settings #(q/smooth 2)
    :setup setup
    :draw draw
    :size [500 500]))

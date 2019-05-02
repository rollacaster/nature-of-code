(ns nature-of-code.01-vectors.exercise-01-08
  (:require [quil.core :as q]))

(defn add [v1 v2]
  (vector (+ (first v1) (first v2))
          (+ (second v1) (second v2))))

(defn sub [v1 v2]
  (vector (- (first v1) (first v2))
          (- (second v1) (second v2))))

(defn mult [v1 n] (vector (* (first v1) n) (* (second v1) n)))

(defn div [v1 n] (vector (/ (first v1) n) (/ (second v1) n)))

(defn mag [[x y]] (Math/sqrt (+ (* x x) (* y y))))

(defn normalize [v]
  (let [m (mag v)]
    (when (not (= m 0)) (div v m))))

(defn limit [[x y] top]
  (if (> (mag [x y]) top)
    (mult (normalize [x y]) top)
    [x y]))

(def location (atom [250 250]))
(def velocity (atom [0 0]))
(def acceleration (atom [0 0]))

(defn setup []
  (q/background 255))

(defn draw []
  (q/clear)
  (q/background 255)
  (let [mouse [(q/mouse-x) (q/mouse-y)]
        direction (sub mouse @location)
        direction-normalized (normalize direction)
        direction-halfed (mult direction-normalized 0.5)
        acceleration direction-halfed
        velocity (swap! velocity #(limit (add % acceleration)
                                         (q/map-range (mag direction) 0 500 5 10)))
        location (swap! location #(add % velocity))]
    (q/fill 0)
    (q/ellipse (first location) (second location) 16 16)))

(defn run []
  (q/defsketch mouse-acceleration
    :title "mouse-acceleration"
    :settings #(q/smooth 2)
    :setup setup
    :draw draw
    :size [500 500]))

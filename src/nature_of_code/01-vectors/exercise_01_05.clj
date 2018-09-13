(ns nature-of-code.01-vectors.exercise-01-05
  (:require [quil.core :as q]))

(defn add [v1 v2]
  (vector (+ (first v1) (first v2))
          (+ (second v1) (second v2))))

(def location (atom [250 250]))
(def velocity (atom [0 0]))
(def acceleration (atom [0 0]))

(defn setup []
  (q/background 255))

(defn draw []
  (q/clear)
  (q/background 255)
  (let [velocity (swap! velocity #(add % @acceleration))
        location (swap! location #(add % velocity))]
    (q/fill 0)
    (q/ellipse (first location) (second location) 16 16)))

(defn on-key-down []
  (cond (= (q/key-as-keyword) :w) (swap! acceleration #(vector (first %) (- (second %) 0.001)))
        (= (q/key-as-keyword) :s) (swap! acceleration #(vector (first %) (+ (second %) 0.001)))
        (= (q/key-as-keyword) :a) (swap! acceleration #(vector (- (first %) 0.001) (second %)))
        (= (q/key-as-keyword) :d) (swap! acceleration #(vector (+ (first %) 0.001) (second %)))))

(q/defsketch car
  :title "car"
  :settings #(q/smooth 2)
  :setup setup
  :key-pressed on-key-down
  :draw draw
  :size [500 500])

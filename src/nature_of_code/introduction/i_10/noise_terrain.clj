(ns nature-of-code.introduction.i-10.noise-terrain
  (:require [quil.core :refer :all]))
(def x (atom 0))
(defn draw
  "Example usage of with-translation and with-rotation."
  []
  (clear)
  (fill 0 0 255)
  (stroke 127)
  (stroke-weight 5)
  (rotate-z 0.1)
  (let [yoff (atom 0.0)]
    (doseq [x (range 0 (/ (width) 50))]
      (doseq [y (range 0 (/ (height) 50))]
        (swap! yoff #(+ 0.01 %))
        (begin-shape)
        (vertex (* x  0) (+ (/ (width) 2) (map-range (noise @yoff) 0 1 0 100)) (* y  0))
        (vertex (* x 50) (+ (/ (width) 2) (map-range (noise @yoff) 0 1 0 100)) (* y  0))
        (vertex (* x 50) (+ (/ (width) 2) (map-range (noise @yoff) 0 1 0 100)) (* y 50))
        (vertex (* x  0) (+ (/ (width) 2) (map-range (noise @yoff) 0 1 0 100)) (* y 50)))))
  (end-shape :close))

(defn setup []
  "Runs once."
  (fill 226)
(frame-rate 10))

#_(defsketch noise-terrain
  :title "noise-terrain"
  :settings #(smooth 2)
  :setup setup
  :draw draw
  :size [500 500]
  :renderer :opengl)

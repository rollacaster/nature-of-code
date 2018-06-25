(ns nature-of-code.introduction.i-4.gaussian-distribution
  (:require [quil.core :as q]))

(defn setup []
  )

(defn draw []
  (let [num (q/random-gaussian)
        sd 60
        mean 320
        x (+ (* sd num) mean)]
    (q/stroke 0)
    (q/fill 255 10)
    (q/ellipse x 180 16 16)))

(q/defsketch gaussian-distribution
  :title "Gaussian distribution"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])



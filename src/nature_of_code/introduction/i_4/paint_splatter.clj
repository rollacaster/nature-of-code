(ns nature-of-code.introduction.i-4.paint-splatter
  (:require [quil.core :as q]))

(defn setup []
  )

(defn draw []
  (let [sd 100
        mean (/ (q/width) 2)
        x (+ (* (q/random-gaussian) sd) mean)
        y (+ (* (q/random-gaussian) sd) mean)
        ]
    (q/fill (+ (* (q/random-gaussian) 50) 128)
            (+ (* (q/random-gaussian) 50) 128)
            (+ (* (q/random-gaussian) 50) 255))
    (q/ellipse x y 10 10)))

(q/defsketch paint-splatter
  :title "Paint Splatter"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

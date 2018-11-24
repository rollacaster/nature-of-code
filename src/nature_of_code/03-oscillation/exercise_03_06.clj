(ns nature-of-code.03-oscillation.exercise-03-06
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  )

(defn draw []
  (q/background 255)
  (let [amplitude 200.0
        period 120.0
        x (* amplitude (q/cos (/ (* q/TWO-PI (q/frame-count)) period)))
        y (* 60 (q/abs (q/sin (/ (* q/TWO-PI (q/frame-count)) period))))]
    (q/push-matrix)
    (q/translate (/ (q/width) 2) 0)
    (q/line 0 0 x (+ y (/ (q/height) 2)))
    (q/ellipse x (+ y (/ (q/height) 2)) 80 80)
    (q/pop-matrix)))

(q/defsketch weight
  :title "weight"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

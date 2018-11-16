(ns nature-of-code.03-oscillation.exercise-03-01
  (:require [quil.core :as q]))

(def rotation (atom 0))

(defn setup []
  )

(defn draw []
  (q/background 255)
  (q/fill 0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/rotate (q/radians (swap! rotation #(+ 1 %))))
  (q/line 0 -100 0 100)
  (q/ellipse 0 -100 20 20)
  (q/ellipse 0 100 20 20))

(q/defsketch rotate-baton
  :title "rotate-baton"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])



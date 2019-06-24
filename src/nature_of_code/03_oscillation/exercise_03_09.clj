(ns nature-of-code.03-oscillation.exercise-03-09
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  (map #(hash-map :angle (* % angleVel) :x (* % 24)) (range (/ (q/width) 24))))

(defn draw [angles]
  (q/background 255)
  (doseq [{:keys [x angle]} angles]
    (let [y (q/map-range (q/noise angle) 0 1 0 (q/height))]
      (q/stroke 0)
      (q/fill 0 50)
      (q/ellipse x y 48 48))))

(defn update-state [state]
  (map #(update % :angle + 0.02) state))

(defn run []
  (q/defsketch perlin-wave
    :title "perlin-wave"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))

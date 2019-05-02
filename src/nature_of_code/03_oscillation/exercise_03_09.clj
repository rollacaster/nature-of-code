(ns nature-of-code.03-oscillation.exercise-03-09
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(def startAngle (atom 0))
(def angleVel 0.1)

(defn setup []
  )

(for [x (range 2)]
    (println "hi" x))

(defn draw []
  (q/background 255)
  (swap! startAngle #(+ % 0.02))
  (let [angle (atom @startAngle)]
    (doseq [i (range (/ (q/width) 24))
            :let [x (* i 24)]]
      (let [y (q/map-range (q/noise @angle) 0 1 0 (q/height))]
        (q/stroke 0)
        (q/fill 0 50)
        (q/ellipse x y 48 48)
        (swap! angle #(+ % angleVel))))))

(defn run []
  (q/defsketch perlin-wave
    :title "perlin-wave"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

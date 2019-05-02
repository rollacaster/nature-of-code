(ns nature-of-code.03-oscillation.exercise-03-04
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(def radius (atom 0))
(def theta (atom 0))

(defn setup []
  (q/background 255))

(defn draw []
  (q/no-stroke)
  (q/fill 0)
  (let [radius (swap! radius #(+ 0.05 %)) 
        x (* radius (q/cos @theta))
        y (* radius (q/sin @theta))]
    (q/ellipse (+ x (/ (q/width) 2)) (+ y (/ (q/height) 2)) 16 16))
  (swap! theta #(+ 0.01 %)))

(defn run []
  (q/defsketch spiral
    :title "spiral"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :size [640 360]))

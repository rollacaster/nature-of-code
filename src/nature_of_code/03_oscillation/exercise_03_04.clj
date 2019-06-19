(ns nature-of-code.03-oscillation.exercise-03-04
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  (q/background 255)
  {:radius 0
   :theta 0})

(defn draw [{:keys [radius theta]}]
  (q/no-stroke)
  (q/fill 0)
  (let [x (* radius (q/cos theta))
        y (* radius (q/sin theta))]
    (q/ellipse (+ x (/ (q/width) 2)) (+ y (/ (q/height) 2)) 16 16)))

(defn update-state [state]
  (-> state
      (update :theta + 0.05)
      (update :radius + 0.05)))

(defn run []
  (q/defsketch spiral
    :title "spiral"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :size [640 360]))

(ns nature-of-code.08-fractals.exercise-08-07
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn branch [theta len]
  (q/stroke-weight (* 0.05 len))
  (q/line 0 0 0 (- len))
  (q/translate 0 (- len))
  (let [new-len (* len 0.66)]
    (if (> new-len 2)
      (do
        (q/push-matrix)
        (q/rotate theta)
        (branch theta new-len)
        (q/pop-matrix)
        (q/push-matrix)
        (q/rotate (- theta))
        (branch theta new-len)
        (q/pop-matrix)))))

(defn setup []
  )

(defn update-state [state]
  )

(defn draw [state]
  (q/clear)
  (q/background 255)
  (q/translate (/ (q/width) 2) (q/height))
  (let [theta (q/map-range (q/mouse-x) 0 (q/width) 0 q/HALF-PI)]
    (branch theta (/ (q/height) 3))))

(q/defsketch tree
  :title "tree"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :update update-state
  :features [:no-bind-output]
  :size [700 500])

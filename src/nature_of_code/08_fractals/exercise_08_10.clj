(ns nature-of-code.08-fractals.exercise-08-10
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn branch [n len]
  (q/stroke-weight (* 0.05 len))
  (q/line 0 0 0 (- len))
  (q/translate 0 (- len))
  (let [new-len (* len 0.66)]
    (if (> new-len 2)
      (do
        (let [theta (q/map-range (q/noise n) 0 1 (- q/HALF-PI) q/HALF-PI)]
          (q/push-matrix)
          (q/rotate theta)
          (branch (+ n 0.2) new-len)
          (q/pop-matrix)
          (q/push-matrix)
          (q/rotate (- theta))
          (branch (+ n 0.1) new-len)
          (q/pop-matrix))))))

(defn setup []
  0)

(defn update-state [state]
  (+ 0.001 state))

(defn draw [state]
  (q/clear)
  (q/background 255)
  (q/translate (/ (q/width) 2) (q/height))
  (let [theta (q/map-range (q/mouse-x) 0 (q/width) 0 q/HALF-PI)]
    (branch state (/ (q/height) 3))))

(defn run []
  (q/defsketch tree
    :title "tree"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :display 1
    :size [700 500]))

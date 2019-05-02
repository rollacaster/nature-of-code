(ns nature-of-code.06-autonomous-agents.exercise-06-10
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  {:path {:r 20
          :start [0 (/ (q/height) 3)]
          :end [(q/width) (* 2 (/ (q/height) 3))]}})

(defn update-state [state]
  state)

(defn draw-path [{:keys [r start end]}]
  (q/stroke-weight (* r 2))
  (q/stroke 0 100)
  (apply q/line (concat start end))
  (q/stroke-weight 1)
  (q/stroke 0)
  (apply q/line (concat start end)))

(defn draw [{:keys [path]}]
  (q/background 255)
  (draw-path path))

(defn run []
  (q/defsketch path-following
    :title "path-following"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))

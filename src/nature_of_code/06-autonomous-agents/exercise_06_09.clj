(ns nature-of-code.06-autonomous-agents.exercise-06-09
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn setup []
  [0 -100])

(defn round [number]
  (double (/ (Math/round (* 100 number)) 100)))

(defn rotate [[x y]]
  (let [r (Math/sqrt (+ (* x x) (* y y)))
        theta (q/atan2 y x)]
    [(* r (q/cos (+ 0.01 theta)))
     (* r (q/sin (+ 0.01 theta)))]))

(defn update-state [state]
  (rotate state))

(defn draw [state]
  (q/background 230)
  (let [[x1 y1 :as v1] [100 0]
        [x2 y2 :as v2] state]
    (q/fill 0)
    (q/text-size 20)
    (q/text (str (round (v/angle-between v1 v2)) " radians") 100 100)
    (q/text (str (Math/ceil (* (v/angle-between v1 v2) (/ 180 q/PI))) " degrees") 100 125)
    (q/translate (/ (q/width) 2) (/ (q/height) 2))
    (q/line 0 0 x1 y1)
    (q/line 0 0 x2 y2)))

(q/defsketch vector-angle
  :title "vector-angle"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :update update-state
  :features [:no-bind-output]
  :size [700 500])

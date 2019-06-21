(ns nature-of-code.03-oscillation.exercise-03-07
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  '({:angle [0 0]
     :velocity [0 0.03]
     :amplitude [-30 -30]
     :location [-50 -50]}
    {:angle [0 0]
     :velocity [0 -0.03]
     :amplitude [30 30]
     :location [50 -50]}
    {:angle [0 0]
     :velocity [0 0.03]
     :amplitude [0 -30]
     :location [-100 0]}
    {:angle [0 0]
     :velocity [0 0.03]
     :amplitude [0 30]
     :location [100 0]}
    {:angle [0 0]
     :velocity [0 0.03]
     :amplitude [-30 -30]
     :location [-50 50]}
    {:angle [0 0]
     :velocity [0 0.03]
     :amplitude [30 30]
     :location [50 50]}))

(defn draw-foot [{[a1 a2] :angle [am1 am2] :amplitude [l1 l2] :location}]
  (let [x (+ l1 (* am1 (q/cos a1)))
        y (+ l2 (* am2 (q/cos a2)))]
    (q/push-matrix)
    (q/stroke 0)
    (q/fill 175)
    (q/translate (/ (q/width) 2) (/ (q/height) 2))
    (q/line 0 0 x y)
    (q/ellipse x y 20 20)
    (q/pop-matrix)))

(defn draw [state]
  (q/background 255)
  (doseq [foot state]
    (draw-foot foot)))

(defn update-foot [{:keys [velocity] :as foot}]
  (update foot :angle v/add velocity))

(defn update-state [state]
  (map update-foot state))

(defn run []
  (q/defsketch insect
    :title "insect"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :update update-state
    :draw draw
    :features [:no-bind-output]
    :size [500 500]))

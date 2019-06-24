(ns nature-of-code.03-oscillation.exercise-03-08
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  (list
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [-30 -30]
    :location [-50 -50]}
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [30 30]
    :location [50 -50]}
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [0 -30]
    :location [-100 0]}
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [0 30]
    :location [100 0]}
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [-30 -30]
    :location [-50 50]}
   {:angle [0 0]
    :velocity [0 0]
    :acceleration [0 0.03]
    :amplitude [30 30]
    :location [50 50]}))

(defn draw [state]
  (q/background 255)
  (doseq [{[a1 a2] :angle
           [am1 am2] :amplitude
           [l1 l2] :location} state]
    (let [x (+ l1 (* am1 (q/cos a1)))
          y (+ l2 (* am2 (q/cos a2)))]
      (q/push-matrix)
      (q/stroke 0)
      (q/fill 175)
      (q/translate (/ (q/width) 2) (/ (q/height) 2))
      (q/line 0 0 x y)
      (q/ellipse x y 20 20)
      (q/pop-matrix))))

(defn update-oscilliator [{:keys [acceleration velocity] :as o}]
  (-> o
      (update :velocity v/add (v/add acceleration [0.00002 0.00002]))
      (update :angle v/add (v/add acceleration velocity))
      (assoc :acceleration [0 0])))

l(defn update-state [state]
  (map update-oscilliator state))


(defn run []
  (q/defsketch angular-oscilliate
    :title "angular-oscilliate"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :update update-state
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

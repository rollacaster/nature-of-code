(ns nature-of-code.03-oscillation.exercise-03-08
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def oscilators (atom '({:angle [0 0]
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
                         :location [50 50]})))

(defn setup []
  )

(defn draw []
  (q/background 255)
  (doseq [{:keys [angle velocity amplitude location]}
          (swap! oscilators
                 #(map (fn [o]
                         (-> o
                             (update :velocity (fn [v] (v/add v (:acceleration (assoc o :acceleration (v/add (:acceleration o) [0.00002 0.00002]))))))
                             (update :angle (fn [a] (v/add a (v/add (:acceleration o)(:velocity o)))))
                             (assoc :acceleration [0 0])))
                       %))]
    (let [[a1 a2] angle
          [am1 am2] amplitude
          [l1 l2] location
          x (+ l1 (* am1 (q/cos a1)))
          y (+ l2 (* am2 (q/cos a2)))]
      (q/push-matrix)
      (q/stroke 0)
      (q/fill 175)
      (q/translate (/ (q/width) 2) (/ (q/height) 2))
      (q/line 0 0 x y)
      (q/ellipse x y 20 20)
      (q/pop-matrix))))

(q/defsketch angular-oscilliate
  :title "angular-oscilliate"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [500 500])
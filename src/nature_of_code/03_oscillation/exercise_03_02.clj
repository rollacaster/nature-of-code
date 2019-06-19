(ns nature-of-code.03-oscillation.exercise-03-02
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))

(defn setup []
  (q/no-stroke)
  {:location [(* (q/width) 0.1) (* (q/height) 0.8)]
   :velocity [0 0]
   :acceleration [0 0]
   :aAcceleration 0
   :aVelocity 0
   :angle 0
   :mass 20})

(defn compute-position [mover]
  (-> mover
      (update :velocity #(v/add (:acceleration mover) %))
      (update :location #(v/add % (v/add (:acceleration mover) (:velocity mover))))
      (update :aVelocity #(+ (:aAcceleration mover) %))
      (update :angle #(+ (:aAcceleration mover) (:aVelocity mover) %))
      (assoc :aAcceleration 0)
      (assoc :acceleration [0 0])))

(defn shoot [{:keys [velocity]}]
  (if (= (v/mag velocity) 0)
    [3 3]
    [0 0]))

(defn update-cannonball [cannonball]
  (-> cannonball
      (m/apply-force (shoot cannonball))
      compute-position))

(defn update-state [cannonball]
  (update-cannonball cannonball))

(defn draw-cannon []
  (q/with-translation [(* (q/width) 0.1) (* (q/height) 0.8)]
    (q/with-fill [0 0 0]
      (q/rect -15 5 30 20)
      (q/with-rotation [(q/radians 225)]
        (q/ellipse 0 0 20 20)
        (q/rect -10 0 20 20)))))

(defn draw-cannonball [{[x y] :location}]
  (q/with-fill [255 0 0]
    (q/ellipse x y 30 30)))

(defn draw [cannonball]
  (q/clear)
  (draw-cannonball cannonball)
  (draw-cannon))

(defn run []
  (q/defsketch cannonball
    :title "cannonball"
    :settings #(q/smooth 2)
    :draw draw
    :setup setup
    :update update-state
    :middleware [md/pause-on-error md/fun-mode]
    :size [800 500]))

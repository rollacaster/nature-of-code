(ns nature-of-code.03-oscillation.exercise-03-12
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn create-pendulum [origin r]
  {:location [0 0] :origin origin :r r :angle (/ q/PI 4.0) :damping 0.995
   :a-velocity 0.0 :a-acceleration 0.0})

(defn update-pendulum [{:keys [angle a-velocity r damping origin] :as pendulum}]
  (let [gravity 0.4
        a-acceleration (* (/ (- gravity) r) (q/sin angle))
        a-velocity (+ a-velocity a-acceleration)
        angle (+ angle a-velocity)
        location (v/add (vector (* r (q/sin angle)) (* r (q/cos angle))) origin)]
    (-> pendulum
        (assoc :a-acceleration a-acceleration)
        (assoc :angle angle)
        (assoc :origin origin)
        (assoc :location location)
        (assoc :a-velocity (* a-velocity damping)))))

(defn draw-pendulum [{:keys [r angle] [ox oy] :origin [x y] :location}]
  (q/stroke 0)
  (q/line ox oy x y)
  (q/ellipse x y 16 16))

(defn setup []
  (list
   (create-pendulum [(/ (q/width) 2) 0] 100.0)
   (create-pendulum [(/ (q/width) 2) 100] 100.0)
   (create-pendulum [(/ (q/width) 2) 200] 100.0)))

(defn update-state [pendulums]
  (map-indexed
   (fn [idx pendulum]
     (update-pendulum (if (= idx 0)
                        pendulum
                        (assoc pendulum :origin (:location (nth pendulums (dec idx)))))))
   pendulums))

(defn draw [pendulums]
  (q/background 255)
  (doseq [pendulum pendulums]
    (draw-pendulum pendulum)))

(defn run []
  (q/defsketch multi-pendulum
    :title "multi-pendulum"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :update update-state
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :display 1
    :size [700 500]))

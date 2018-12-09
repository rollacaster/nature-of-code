(ns nature-of-code.03-oscillation.exercise-03-12
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn create-pendulum [origin r]
  {:location [] :origin origin :r r :angle (/ (Math/PI) 4.0) :damping 0.995
   :aVelocity 0.0 :aAcceleration 0.0})

(defn update-pendulum [{:keys [angle aVelocity r damping origin] :as pendulum}]
  (let [gravity 0.4
        aAcceleration (* (/ (- gravity) r) (q/sin angle))
        aVelocity (+ aVelocity aAcceleration)
        angle (+ angle aVelocity)
        location (v/add (vector (* r (q/sin angle)) (* r (q/cos angle))) origin)]
    (-> pendulum
        (assoc :aAcceleration aAcceleration)
        (assoc :angle angle)
        (assoc :origin origin)
        (assoc :location location)
        (update :aVelocity (fn [_] (* aVelocity damping))))))

(defn draw-pendulum [{:keys [r angle origin location]}]
  (q/stroke 0)
  (q/line (first origin) (second origin) (first location) (second location))
  (q/ellipse (first location) (second location) 16 16))

(def pendulums (atom (list
                      (create-pendulum [350 0] 100.0)
                      (create-pendulum [350 100] 100.0))))


(def pendulum (atom (create-pendulum [350 0] 100.0)))


(defn setup []
  )

(defn update-pendulums [pendulums]
  (reduce (fn [pendulums pendulum]
            (conj pendulums
                  (update-pendulum (assoc pendulum :origin (if (= (:origin pendulum) [350 0])
                                                             (:origin pendulum)
                                                             (:location (last pendulums)))))))
          ()
          pendulums))

(defn draw []
  (q/background 255)
  (doseq [pendulum (swap! pendulums update-pendulums)]
    (draw-pendulum pendulum)))

(q/defsketch multi-pendulum
  :title "multi-pendulum"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

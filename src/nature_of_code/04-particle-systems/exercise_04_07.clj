(ns nature-of-code.04-particle-systems.exercise-04-07
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 2)]
   :acceleration [0 0.05]
   :lifespan 255.0
   :aAcceleration 0.1
   :aVelocity 0.0
   :angle 0.0})

(defn update-particle [{:keys [acceleration velocity location lifespan
                               aVelocity aAcceleration angle] :as particle}]
  (let [velocity (v/add velocity acceleration)
        location (v/add velocity location)
        lifespan (- lifespan 2.0)
        aVelocity (+ aVelocity aAcceleration)
        angle (+ aVelocity angle)]
    (-> particle
        (assoc :velocity velocity)
        (assoc :location location)
        (assoc :lifespan lifespan)
        (assoc :aVelocity aVelocity)
        (assoc :angle angle)
        (assoc :aAcceleration 0.0))))

(defn display-particle [{:keys [lifespan] [x y] :location :as particle}]
  (q/stroke 0 lifespan)
  (q/fill 0 lifespan)
  (q/ellipse x y 8 8)
  particle)

(defn display-confetti [{:keys [lifespan angle] [x y] :location :as particle}]
  (let [theta (q/map-range x 0.0 (q/width) 0 (* q/TWO-PI 2.0))]
    (q/rect-mode :center)
    (q/fill 175 lifespan)
    (q/stroke 0 lifespan)
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/rect 0 0 8 8)
    (q/pop-matrix)
    particle))

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn run [particle]
  (-> particle
      update-particle
      display-confetti))

(defn setup []
  )

(def particle (atom (create-particle [250 250])))

(defn draw []
  (q/background 255)
  (when (is-dead (swap! particle run))
    (def particle (atom (create-particle [250 250])))))

(q/defsketch particle
  :title "particle"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

(ns nature-of-code.04-particle-systems.exercise-04-01
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 2)]
   :acceleration [0 0.05]
   :lifespan 255.0})

(defn update-particle [{:keys [acceleration velocity location lifespan] :as particle}]
  (let [velocity (v/add velocity acceleration)
        location (v/add velocity location)
        lifespan (- lifespan 2.0)]
    (-> particle
        (assoc :velocity velocity)
        (assoc :location location)
        (assoc :lifespan lifespan))))

(defn apply-force [particle force]
  (update particle :acceleration #(v/add % force)))

(defn display [{:keys [lifespan] [x y] :location}]
  (q/stroke 0 lifespan)
  (q/fill 0 lifespan)
  (q/ellipse x y 8 8))

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn setup []
  (create-particle [250 250]))

(defn update-state [particle]
  (if (is-dead particle)
    (create-particle [250 250])
    (-> particle
        (apply-force [-0.01 0.01])
        update-particle)))

(defn draw [particle]
  (q/background 255)
  (display particle))

(defn run []
  (q/defsketch particle-force
    :title "particle-force"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :display 1
    :size [700 500]))

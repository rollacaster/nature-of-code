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

(defn display [{:keys [lifespan] [x y] :location :as particle}]
  (q/stroke 0 lifespan)
  (q/fill 0 lifespan)
  (q/ellipse x y 8 8)
  particle)

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn run [particle]
  (-> particle
      (apply-force [-0.01 0.01])
      update-particle
      display))

(defn setup []
  )

(def particle (atom (create-particle [250 250])))

(defn draw []
  (q/background 255)
  (when (is-dead (swap! particle run))
    (def particle (atom (create-particle [250 250])))))

(q/defsketch particle-force
  :title "particle-force"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

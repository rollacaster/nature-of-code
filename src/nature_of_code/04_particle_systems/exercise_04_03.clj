(ns nature-of-code.04-particle-systems.exercise-04-03
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(def particles (atom ()))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 2)]
   :acceleration [0 0.05]
   :lifespan 255.0
   :aAcceleration 0.0
   :aVelocity 0.1
   :angle 0.0})

(defn update-particle [{:keys [acceleration velocity location lifespan
                               aVelocity aAcceleration angle]
                        :as particle}]
  (let [velocity (v/add velocity acceleration)
        location (v/add velocity location)
        lifespan (- lifespan 2.0)
        aVelocity (+ aVelocity aAcceleration)
        angle (+ aVelocity angle)]
    (-> particle
        (assoc :velocity velocity)
        (assoc :location location)
        (assoc :acceleration [0 0])
        (assoc :lifespan lifespan)
        (assoc :aVelocity aVelocity)
        (assoc :angle (+ aVelocity angle))
        (assoc :aAcceleration 0))))

(defn apply-force [particle force]
  (update particle :acceleration #(v/add % force)))

(defn display [{:keys [lifespan angle] [x y] :location :as particle}]
  (q/push-matrix)
  (q/rect-mode :center)
  (q/translate x y)
  (q/rotate angle)
  (q/stroke 0 lifespan)
  (q/fill 127 0 0 lifespan)
  (q/rect 0 0 8 8)
  (q/pop-matrix)
  particle)

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn run [particle]
  (-> particle
      #_(apply-force [0.1 0])
       update-particle
      display))

(defn setup []
  )

(defn add-origin [origin {:keys [location] :as particle}]
  (assoc particle :location (v/add location origin)))

(defn run-particle-system [particles]
  (doseq [particle (swap! particles
                          #(->> (conj % (add-origin [(q/mouse-x) (q/mouse-y)] (create-particle [0 0])))
                               (map run)
                               (remove is-dead)))]))

(defn draw []
  (q/background 255)
  (run-particle-system particles))

(q/defsketch particle-system-mouse
  :title "particle-system-mouse"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

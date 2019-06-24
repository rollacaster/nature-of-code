(ns nature-of-code.04-particle-systems.exercise-04-13
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))
(defn setup []
  (q/image-mode :center)
  (q/no-stroke)
  {:particles ()
   :image (q/load-image "texture.png")})

(defn create-texture [location]
  {:location location
   :velocity [(* (q/random-gaussian) 0.3) (- (* (q/random-gaussian) 0.3) 1.0)]
   :acceleration [0 0]
   :lifespan 255.0
   :aAcceleration 0.1
   :aVelocity 0.0
   :angle 0.0
   :mass 10
   :strength 5})

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn apply-force [force {:keys [mass acceleration] :as particle}]
  (assoc particle :acceleration (v/add acceleration (v/div force mass))))

(defn update-particle [{:keys [acceleration velocity location lifespan
                               aVelocity aAcceleration angle] :as particle}]
  (let [velocity (v/add velocity acceleration)
        location (v/add velocity location)
        aVelocity (+ aVelocity aAcceleration)
        angle (+ aVelocity angle)]
    (-> particle
        (assoc :velocity velocity)
        (assoc :location location)
        (update :lifespan dec)
        (assoc :aVelocity aVelocity)
        (assoc :angle angle)
        (assoc :aAcceleration 0.0)
        (assoc :acceleration [0 0]))))

(defn update-state [state]
  (let [wind [(q/map-range (q/mouse-x) 0.0 (q/width) -0.2 0.2)
              (q/map-range (q/mouse-y) 0.0 (q/height) -0.2 0.2)]
        origin [(/ (q/width) 2) (* 0.8 (q/height))]]
    (update state :particles
            #(->> (conj % (create-texture origin))
                 (map (partial apply-force wind))
                 (map update-particle)
                 (remove is-dead)))))

(defn draw-particle [{:keys [lifespan angle] [x y] :location :as particle} image]
  (q/fill 255 20 5 lifespan)
  (q/ellipse x y
             (q/map-range lifespan 255 0 100 0)
             (q/map-range lifespan 255 0 100 0)))

(defn draw [{:keys [particles image]}]
  (q/blend-mode :add)
  (q/background 0)
  (doseq [particle particles]
    (draw-particle particle image)))

(defn run []
  (q/defsketch particle
    :title "particle"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [500 500]
    :renderer :opengl))

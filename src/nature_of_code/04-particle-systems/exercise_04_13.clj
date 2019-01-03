(ns nature-of-code.04-particle-systems.exercise-04-13
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def gravity [0 0.3])
(def image nil)
(def particle-system (atom {:particles () :origin [350 250]}))


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

(defn add-particle [ps]
  (update ps :particles #(conj % (create-texture (:origin ps)))))

(defn display-particle [{:keys [lifespan angle] [x y] :location :as particle}]
  (q/image-mode :center)
  (q/color-mode :hsb)
  (q/tint lifespan 230 127)
  (q/image image x y))


(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn apply-force [{:keys [mass acceleration] :as particle} force]
  (assoc particle :acceleration
         (v/add acceleration (v/div force mass))))

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
        (assoc :aAcceleration 0.0)
        (assoc :acceleration [0 0]))))

(defn run-particle-system [{:keys [particles confetti] :as ps}]
  (doseq [particle particles]
    (-> particle
        display-particle))
  (let [wind [(q/map-range (q/mouse-x) 0.0 (q/width) -0.2 0.2)
              (q/map-range (q/mouse-y) 0.0 (q/height) -0.2 0.2)]]
    (-> ps
        (update :particles #(map (fn [particle] (apply-force particle wind)) %))
        (update :particles #(map update-particle %))
        (update :particles #(remove is-dead %)))))

(defn setup []
  (def image (q/load-image "texture-white.png")))

(defn draw []
  (q/blend-mode :add)
  (q/background 0)
  (swap! particle-system (comp
                          add-particle
                          run-particle-system)))

(q/defsketch particle
  :title "particle"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500]
  :renderer :p2d)

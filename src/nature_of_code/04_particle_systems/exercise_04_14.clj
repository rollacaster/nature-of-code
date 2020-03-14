(ns nature-of-code.04-particle-systems.exercise-04-14
  (:require [nature-of-code.vector :as v]
            [nature-of-code.mover :as m]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  (q/image-mode :center)
  (q/color-mode :hsb)
  (q/no-stroke)
  {:particles ()
   :image (q/load-image "resources/images/texture-white.png")})

(defn create-texture [location]
  (assoc (m/create-mover 10 location)
         :velocity [(* (q/random-gaussian) 0.8) (- (q/random-gaussian) 1.0)]
         :lifespan 255.0))

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn apply-force [force {:keys [mass acceleration] :as particle}]
  (assoc particle :acceleration (v/add acceleration (v/div force mass))))

(defn dec-lifespan [particle] (update particle :lifespan (comp dec dec)))

(defn update-state [state]
  (update state :particles
          #(->> (conj % (create-texture [(/ (q/width) 2) (* 0.8 (q/height))]))
                (map (comp m/compute-position dec-lifespan))
                (remove is-dead))))

(defn draw-particle [{:keys [lifespan] [x y] :location} image]
  (q/tint lifespan 255 255)
  (q/image image x y
           (q/map-range lifespan 255 0 100 0)
           (q/map-range lifespan 255 0 100 0)))

(defn draw [{:keys [particles image]}]
  (q/blend-mode :replace)
  (q/background 0)
  (q/color-mode :hsb)
  (doseq [particle particles]
    (draw-particle particle image)))

(defn run []
  (q/defsketch particle-blend
    :title "particle-blend"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [500 500]
    :renderer :opengl))

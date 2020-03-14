(ns nature-of-code.04-particle-systems.exercise-04-12
  (:require [nature-of-code.mover :as m]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn create-particle [location images]
  (assoc (m/create-mover 10 location)
         :velocity [(* (q/random-gaussian) 0.3) (- (* (q/random-gaussian) 0.3) 1.0)]
         :lifespan 255.0
         :mass 10
         :image (rand-nth images)))

(defn setup []
  {:images [(q/load-image "resources/images/sojka.jpg")
            (q/load-image "resources/images/fcb.jpg")
            (q/load-image "resources/images/emacs.png")]
   :particles ()
   :origin [350 250]})

(defn draw-particle [{:keys [lifespan image] [x y] :location}]
  (q/image-mode :center)
  (q/tint 255 lifespan)
  (q/image image x y 20 20))

(defn is-dead [{:keys [lifespan]}] (< lifespan 0.0))
(defn dec-lifespan [particle] (update particle :lifespan (comp dec dec)))

(defn update-state [{:keys [images] :as ps}]
  (-> ps
      (update :particles #(conj % (create-particle (:origin ps) images)))
      (update :particles #(map (comp m/compute-position dec-lifespan) %))
      (update :particles #(remove is-dead %))))

(defn draw [{:keys [particles]}]
  (q/background 255)
  (doseq [particle particles]
    (draw-particle particle)))

(defn run []
  (q/defsketch particle-images
    :title "particle"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :update update-state
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

(ns nature-of-code.04-particle-systems.exercise-04-05
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 1)]
   :acceleration [0 0]
   :lifespan 255.0
   :aAcceleration 0.0
   :aVelocity 0.1
   :angle 0.0})

(defn setup []
  ())

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

(defn is-dead [{:keys [lifespan]}] (< lifespan 0.0))

(defn update-particle-system [{:keys [particles count]
                               [x y] :location
                               :as particle-system}]
  (assoc particle-system
         :count (dec count)
         :particles (let [particles (if (> count 1)
                                      (conj particles (create-particle [x y]))
                                      particles)]
                      (->> particles
                           (map update-particle)
                           (remove is-dead)))))

(defn create-particle-system [x y count]
  {:location [x y]
   :count count
   :particles []})

(defn update-state [particle-systems]
  (map update-particle-system particle-systems))

(defn draw-particle [{:keys [lifespan angle] [x y] :location}]
  (q/push-matrix)
  (q/rect-mode :center)
  (q/translate x y)
  (q/rotate angle)
  (q/stroke 0 lifespan)
  (q/fill 127 0 0 lifespan)
  (q/rect 0 0 8 8)
  (q/pop-matrix))

(defn draw [particle-systems]
  (q/background 255)
  (doseq [particle-system particle-systems]
    (doseq [particle (:particles particle-system)]
      (draw-particle particle))))

(defn mouse-pressed [state {:keys [x y]}]
  (conj state (create-particle-system x y 50)))

(defn run []
  (q/defsketch particles-on-click
    :title "particles-on-click"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :mouse-pressed mouse-pressed
    :size [700 500]))

(ns nature-of-code.04-particle-systems.exercise-04-07
  (:require [nature-of-code.mover :as m]
            [quil.core :as q]
            [quil.middleware :as md]))

;; Instead of using map() to calculate theta, how would you model angular velocity and acceleration?

(defn display-confetti [{:keys [lifespan angle] [x y] :location :as particle}]
  (q/rect-mode :center)
  (q/fill 175 lifespan)
  (q/stroke 0 lifespan)
  (q/push-matrix)
  (q/translate x y)
  (q/rotate angle)
  (q/rect 0 0 8 8)
  (q/pop-matrix)
  particle)

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn setup-particle []
  (assoc (m/create-mover 10 [250 250])
         :lifespan 255
         :velocity [(- (rand 2) 1) (- (rand 2) 2)]
         :acceleration [0 0.05]
         :a-acceleration 0.1))

(defn update-state [particle]
  (m/compute-position
   (if (is-dead particle)
     (setup-particle)
     (update particle :lifespan dec))))

(defn setup []
  (setup-particle))

(defn draw [particle]
  (q/background 255)
  (display-confetti particle))

(defn run []
  (q/defsketch calculate-theta
    :title "calculate-theta"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [800 500]))

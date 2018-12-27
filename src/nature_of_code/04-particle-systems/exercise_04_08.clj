(ns nature-of-code.04-particle-systems.exercise-04-07
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def particle-system (atom {:particles ()
                            :confetti ()
                            :origin [200 200]}))
(defn add-particle [ps]
  (update ps :particles #(conj % (apply (if (> (q/random 1) 0.5)
                                          create-particle
                                          create-confetti)
                                        (list (:origin ps))))))


(defn run-particle-system [{:keys [particles confetti] :as ps}]
  (doseq [particle particles]
    (-> particle
        display-particle))
  (-> ps
      (update :particles #(map update-particle %))
      (update :particles #(remove is-dead %))
      (update :confetti #(map update-particle %))
      (update :confetti #(remove is-dead %))))

(defn create-confetti [location]
  (-> (create-particle location)
      (assoc :type :confetti)))

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

(defmulti display-particle :type)
(defmethod display-particle :confetti [{:keys [lifespan angle] [x y] :location :as particle}]
  (q/rect-mode :center)
  (q/fill 175 lifespan)
  (q/stroke 0 lifespan)
  (q/push-matrix)
  (q/translate x y)
  (q/rotate angle)
  (q/rect 0 0 8 8)
  (q/pop-matrix)
  particle)
(defmethod display-particle :default [{:keys [lifespan] [x y] :location :as particle}]
  (q/stroke 0 lifespan)
  (q/fill 0 lifespan)
  (q/ellipse x y 8 8)
  particle)

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn setup []
  )

(defn draw []
  (q/background 255)
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
  :size [700 500])

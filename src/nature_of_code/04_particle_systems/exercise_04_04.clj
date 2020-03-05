(ns nature-of-code.04-particle-systems.exercise-04-04
  (:require [nature-of-code.04-particle-systems.exercise-04-03 :as p]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  {:spaceship (m/create-mover 50 [250 250])
   :angle 0
   :particles ()})

(defn add-origin [origin {:keys [location] :as particle}]
  (assoc particle :location (v/add location origin)))

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

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

(defn apply-force [force particle]
  (update particle :acceleration #(v/add % force)))

(defn add-particle [particles]
  (if (and (q/key-pressed?) (= (q/key-as-keyword) :up))
    (conj particles (add-origin [-40 0] (p/create-particle [0 0])))
    particles))

(defn update-state [state]
  (-> state
      (update :particles #(->> %
                               add-particle
                               (map (comp update-particle (partial apply-force [-0.05 0])))
                               (remove is-dead)))
      (update :spaceship #(-> %
                             (m/apply-force (v/mult (:velocity %) -0.2))
                             m/move-through
                             (m/compute-position)))))

(defn draw [{:keys [particles spaceship]}]
  (q/background 255)
  (q/fill 126)
  (q/stroke-weight 2)
  (let [{:keys [mass angle] [x y] :location} spaceship]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/triangle (/ mass 2) 0 (- (/ mass 2)) (- (/ mass 2)) (- (/ mass 2)) (/ mass 2))
    (q/rect (- (- (/ mass 2)) 2) 5 5 5)
    (q/rect (- (- (/ mass 2)) 2) -5 5 5)
    (doseq [particle particles]
      (display particle))
    (q/pop-matrix)))

(defn key-pressed [state {:keys [key]}]
  (update state :spaceship #(cond (= key :left) (update % :angle (fn [angle] (+ angle 0.2)))
                                  (= key :right) (update % :angle (fn [angle] (- angle 0.2)))
                                  (= key :up)
                                  (assoc % :acceleration [(q/cos (:angle %))
                                                          (q/sin (:angle %))])
                                  :else %)))

(defn run []
  (q/defsketch asteriods
    :title "asteriods"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :key-pressed key-pressed
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [900 500]))


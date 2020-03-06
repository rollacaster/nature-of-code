(ns nature-of-code.04-particle-systems.exercise-04-04
  (:require [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 1)]
   :acceleration [0 0]
   :lifespan 255.0
   :aAcceleration 0.0
   :aVelocity 0.1
   :angle 0.0})

(defn setup []
  {:spaceship (m/create-mover 50 [250 250])
   :particles ()})

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

(defn apply-force [particle force]
  (update particle :acceleration #(v/add % force)))

(defn add-particle [{:keys [angle]} particles]
  (if (and (q/key-pressed?) (= (q/key-as-keyword) :up))
    (conj particles (setup-particle [(* 40 (q/cos (+ angle q/PI)))
                                     (* 40 (q/sin (+ angle q/PI)))]))
    particles))

(defn update-state [{:keys [spaceship] :as state}]
  (-> state
      (update :particles #(->> %
                               (add-particle spaceship)
                               (map (fn [particle]
                                      (-> particle
                                          (apply-force [(* 0.05 (q/cos (+ (:angle spaceship) q/PI)))
                                                        (* 0.05 (q/sin (+ (:angle spaceship) q/PI)))])
                                          update-particle)))
                               (remove is-dead)))
      (update :spaceship #(-> %
                             (m/apply-force (v/mult (:velocity %) -0.2))
                             m/move-through
                             (m/compute-position)))))


(defn draw-spaceship [{:keys [mass]}]
  (q/triangle (/ mass 2) 0 (- (/ mass 2)) (- (/ mass 2)) (- (/ mass 2)) (/ mass 2))
  (q/rect (- (- (/ mass 2)) 2) 5 5 5)
  (q/rect (- (- (/ mass 2)) 2) -5 5 5))

(defn draw-particle [{:keys [lifespan angle] [x y] :location :as particle}]
  (q/push-matrix)
  (q/rect-mode :center)
  (q/translate x y)
  (q/rotate angle)
  (q/with-stroke [0 lifespan]
    (q/with-fill [127 0 0 lifespan]
      (q/rect 0 0 8 8)))
  (q/pop-matrix)
  particle)

(defn draw [{:keys [particles spaceship]}]
  (q/background 255)
  (q/fill 126)
  (q/stroke-weight 2)
  (let [{:keys [angle] [x y] :location} spaceship]
    (q/push-matrix)
    (q/translate x y)
    (doseq [particle particles]
      (draw-particle particle))
    (q/pop-matrix)
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (draw-spaceship spaceship)
    (q/pop-matrix)))

(defn key-pressed [state {:keys [key]}]
  (update state :spaceship #(cond (= key :left) (update % :angle (fn [angle] (mod (+ angle 0.2) q/TWO-PI)))
                                  (= key :right) (update % :angle (fn [angle] (mod (- angle 0.2) q/TWO-PI)))
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


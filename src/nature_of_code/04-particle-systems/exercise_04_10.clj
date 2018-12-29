(ns nature-of-code.04-particle-systems.exercise-04-10
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def gravity [0 0.3])

(def particle-system (atom {:particles () :origin [200 200]}))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 2)]
   :acceleration [0 0]
   :lifespan 255.0
   :aAcceleration 0.1
   :aVelocity 0.0
   :angle 0.0
   :mass 10
   :strength 5})

(defn create-confetti [location]
  (-> (create-particle location)
      (assoc :type :confetti)))

(defn add-particle [ps]
  (update ps :particles #(conj % (apply (if (> (q/random 1) 0.5)
                                          create-particle
                                          create-confetti)
                                        (list (:origin ps))))))

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

(defn repel [{:keys [location strength]} particle]
  (let [dir (v/sub location, (:location particle))
        d (q/constrain (v/mag dir) 5 100)
        force (/ (* -1 strength) (* d d))]
    (v/mult (v/normalize dir) force)))

(defn run-particle-system [{:keys [particles confetti] :as ps}]
  (doseq [particle particles]
    (-> particle
        display-particle))
  (-> ps
      (update :particles
              #(map (fn [particle]
                      (reduce
                       (fn [particle repeller]
                         (apply-force particle (repel repeller particle)))
                       particle
                       %))
                    %))
      (update :particles #(map (fn [particle] (apply-force particle gravity)) %))
      (update :particles #(map update-particle %))
      (update :particles #(remove is-dead %))))

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

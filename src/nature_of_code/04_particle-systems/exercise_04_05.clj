(ns nature-of-code.04-particle-systems.exercise-04-05
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def particle-systems (atom ()))

(defn create-particle [location]
  {:location location
   :velocity [(- (rand 2) 1) (- (rand 2) 1)]
   :acceleration [0 0]
   :lifespan 255.0
   :aAcceleration 0.0
   :aVelocity 0.1
   :angle 0.0})

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

(defn apply-force [force particle]
  (update particle :acceleration #(v/add % force)))

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

(defn is-dead [{:keys [lifespan]}]
  (< lifespan 0.0))

(defn run [particle]
  (-> particle
      update-particle
      display))

(defn setup []
  )

(defn add-origin [origin {:keys [location] :as particle}]
  (assoc particle :location (v/add location origin)))

(defn run-particle-system [particles x y particle-count]
  (doall
   (->> (if (> particle-count 1)
          (conj particles (add-origin [x y] (create-particle [0 0])))
          particles)
        (map run)
        (remove is-dead))))

(defn create-particle-system [x y particle-count]
  {:location [x y]
   :particle-count particle-count
   :particles []})

(defn run-particle-systems [{:keys [particles particle-count]
                             [x y] :location :as particle-system}]
  (-> particle-system
      (update :particle-count dec)
      (assoc :particles (run-particle-system particles x y (dec particle-count)))))

(def particle-systems (atom ()))
(defn draw []
  (q/background 255)
  (doall (swap! particle-systems #(map run-particle-systems %))))

(defn on-mouse-pressed []
  (swap! particle-systems #(conj % (create-particle-system (q/mouse-x)
                                                           (q/mouse-y)
                                                           127))))

(defn run []
  (q/defsketch particle-system-mouse
    :title "particle-system-mouse"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :mouse-pressed on-mouse-pressed
    :size [700 500]))

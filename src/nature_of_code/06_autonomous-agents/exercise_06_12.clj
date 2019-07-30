(ns nature-of-code.06-autonomous-agents.exercise-06-12
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn setup-vehicle [[x y]]
  {:location [x y]
   :velocity [0 0]
   :acceleration [0 0]
   :maxspeed 4.0
   :r 10.0
   :maxforce 1})

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn move-through [mover]
  (let [{:keys [location]} mover
        [x y] location]
    (cond (> x (q/width)) (-> mover (assoc :location [0 y]))
          (< x 0) (-> mover (assoc :location [(q/width) y]))
          (> y (q/height)) (assoc mover :location [x 0])
          (< y 0) (assoc mover :location [x (q/height)])
          :else mover)))

(defn seek [target {:keys [location maxspeed velocity maxforce] :as vehicle}]
  (let [desired-location (v/sub target location)
        max-speed (q/map-range (v/mag desired-location) 0 (q/width) 0 maxspeed)
        desired (v/mult (v/normalize desired-location) max-speed)
        max-force (q/map-range (v/mag desired-location) 0 (q/width) 0 maxforce)
        steer (v/limit (v/sub desired velocity) maxforce)]
    steer))

(defn setup []
  (map (fn [i] (setup-vehicle [(q/random (q/width)) (q/random (q/height))])) (range 100)))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn separate [vehicles {:keys [location maxspeed velocity r] :as vehicle}]
  (let [desired-separation (* r 2)
        {:keys [sum count]} (reduce #(let [diff (v/sub location (:location %2))
                                           d (v/mag diff)]
                                       (if (and (> d 0.0) (< d desired-separation))
                                         {:count (inc (:count %1))
                                          :sum (v/add (:sum %1) (v/div (v/normalize diff) d))}
                                         %1))
                                    {:count 0
                                     :sum [0 0]}
                                    vehicles)]
    (if (> count 0)
      (let [avg-v (v/mult (v/normalize (v/div sum count)) maxspeed)
            steer (v/sub avg-v velocity)]
        steer)
      [0 0])))

(defn cohese [vehicles {:keys [location maxspeed velocity r] :as vehicle}]
  (let [desired-cohesion (* r 2)
        {:keys [sum count]} (reduce #(let [diff (v/sub location (:location %2))
                                           d (v/mag diff)]
                                       (if (and (> d 0.0) (> d desired-cohesion))
                                         {:count (inc (:count %1))
                                          :sum (v/add (:sum %1) (v/div (v/normalize diff) d))}
                                         %1))
                                    {:count 0
                                     :sum [0 0]}
                                    vehicles)]
    (if (> count 0)
      (let [avg-v (v/mult (v/normalize (v/div sum count)) maxspeed)
            steer (v/sub avg-v velocity)]
        (v/mult steer -1))
      vehicle)))

(defn update-state [state]
  (map (fn [v]
         (-> v
             (apply-force (v/mult (separate state v) 1.5))
             (apply-force (v/mult (seek [(q/mouse-x) (q/mouse-y)] v) 0.5))
             move-through
             update-vehicle))
       state))

(defn draw-vehicle [{:keys [r velocity] [x y] :location}]
  (q/ellipse x y 8 8)
  #_(let [theta (+ (q/atan2 (second velocity) (first velocity)) q/HALF-PI)]
    (q/fill 175)
    (q/stroke 0)
    (q/push-matrix)
    (q/translate x y)
    (q/rotate theta)
    (q/begin-shape)
    (q/vertex 0 (* (- r) 2))
    (q/vertex (- r) (* r 2))
    (q/vertex r (* r 2))
    (q/end-shape :close)
    (q/pop-matrix)))

(defn draw [state]
  (q/clear)
  (doseq [vehicle state]
    (draw-vehicle vehicle)))

(defn start []
  (q/defsketch cohesion
    :title "cohesion"
    :settings #(q/smooth 2)
    :middleware [ md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :display 1
    :size [750 500]))

(ns nature-of-code.06-autonomous-agents.exercise-06-01
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(def vehicle (atom
              {:acceleration [0 0]
               :velocity [0 0]
               :location [350 250]
               :r 10.0
               :maxspeed 4.0
               :maxforce 0.1}))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn flee [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/mult (v/normalize (v/sub target location)) maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle (v/mult steer -1))))

(defn keep-inside [{[x y] :location :as vehicle}]
  (assoc vehicle :location [(cond
                              (> x (- (q/width) 20)) (- (q/width) 20)
                              (< x 20) 20
                              :else x)
                            (cond
                              (> y (- (q/height) 20)) (- (q/height) 20)
                              (< y 20) 20
                              :else y)]))

(defn display [{:keys [r velocity]
                [x y] :location}]
  (let [theta (+ (q/atan2 (second velocity) (first velocity)) q/HALF-PI)]
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

(defn draw []
  (q/clear)
  (q/background 200)
  (display (swap! vehicle #(-> %
                               (flee [(q/mouse-x) (q/mouse-y)])
                               keep-inside
                               update-vehicle))))

(q/defsketch fleeing
  :title "fleeing"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

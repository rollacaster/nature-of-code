(ns nature-of-code.06-autonomous-agents.exercise-06-03
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(def goal (atom [0 0]))

(def vehicle (atom
              {:acceleration [0 0]
               :velocity [0 0]
               :location [0 0]
               :r 10.0
               :maxspeed 20.0
               :maxforce 1}))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn seek [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired-location (v/sub target location)
        max-speed (q/map-range (v/mag desired-location) 0 (q/width) 0 maxspeed)
        desired (v/mult (v/normalize desired-location) max-speed)
        max-force (q/map-range (v/mag desired-location) 0 (q/width) 0 maxforce)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

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
  (let [[x y] @goal]
    (q/ellipse x y 16 16))
  (display (swap! vehicle #(-> %
                               (seek @goal)
                               keep-inside
                               update-vehicle))))

(q/defsketch different-speeds
  :title "different-speeds"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :draw draw
  :features [:no-bind-output]
  :mouse-pressed #(reset! goal [(rand-int 700) (rand-int 500)])
  :size [700 500])

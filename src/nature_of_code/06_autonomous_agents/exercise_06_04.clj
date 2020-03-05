(ns nature-of-code.06-autonomous-agents.exercise-06-04
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(def goal (atom [416 250]))

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

(defn move-through [{[x y] :location :as vehicle}]
  (assoc vehicle :location [(cond
                              (> x (q/width)) 0
                              (< x 0) (q/width)
                              :else x)
                            (cond
                              (> y (q/height)) 0
                              (< y 0) (q/height)
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

(defn seek [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/mult (v/normalize (v/sub target location)) maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn display-circle []
  (let [{[x y] :location} @vehicle
        center-x (+ 50 x)
        center-y y
        radius 16]
    (q/ellipse center-x center-y (* radius 2) (* radius 2))
    (q/stroke 255 0 0)
    (q/line center-x center-y (first @goal) (second @goal))
    (swap! goal (fn [goal]
                  (let [angle (q/random q/TWO-PI)]
                    [(+ center-x (* radius (q/cos angle)))
                     (+ center-y (* radius (q/sin angle)))])))))

(defn draw []
  (q/clear)
  (q/background 200)
  (display-circle)
  (let [vehicle (swap! vehicle #(-> %
                               (seek @goal)
                               move-through
                               update-vehicle))
        {[x y] :location} vehicle]
    (display vehicle)))

(defn run []
  (q/defsketch wandering
    :title "wandering"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :draw draw
    :features [:no-bind-output]
    :mouse-pressed #(reset! goal [(rand-int 700) (rand-int 500)])
    :size [700 500]))

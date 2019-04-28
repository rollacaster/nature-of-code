(ns nature-of-code.06-autonomous-agents.exercise-06-07
  (:require [quil.core :as q]
            [quil.applet :as a]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn setup []
  {:vehicles '({:acceleration [0 0]
                :velocity [0 0]
                :location [350 250]
                :r 10.0
                :maxspeed 4.0
                :maxforce 0.1}
               {:acceleration [0 0]
                :velocity [0 0]
                :location [50 50]
                :r 10.0
                :maxspeed 4.0
                :maxforce 0.1}
               {:acceleration [0 0]
                :velocity [0 0]
                :location [700 500]
                :r 10.0
                :maxspeed 4.0
                :maxforce 0.1})
   :flow-field
   (let [xoff (atom 0.0)
         resolution 20
         cols (/ (q/width) resolution)
         rows (/ (q/height) resolution)]
     (for [x (range cols)]
       (do
         (swap! xoff + 0.3)
         (let [yoff (atom 0.0)]
           (for [y (range rows)]
             (let [theta (q/map-range (q/noise @xoff @yoff) 0 1 0 q/TWO-PI)]
               (swap! yoff + 0.3)
               [(q/cos theta) (q/sin theta)]))))))})

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn move-through [{[x y] :location :as vehicle}]
  (assoc vehicle :location [(cond
                              (> x (q/width)) 0
                              (< x 0) (q/width)
                              :else x)
                            (cond
                              (> y (q/height)) 0
                              (< y 0) (q/height)
                              :else y)]))

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn lookup [flow-field {[x y] :location}]
  (let [column (q/constrain (/ x 20) 0 34)
        row (q/constrain (/ y 20) 0 24)]
    (nth (nth flow-field column) row)))

(defn follow [flow-field {:keys [location maxspeed velocity maxforce] :as vehicle}]
  (let [target (lookup flow-field vehicle)
        desired (v/mult target maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn rotate-arrows [flow-field]
  (map
   (fn [line]
     (map
      (fn [[x y]]
        [(q/cos (mod (+ 0.01 (q/atan2 y x)) q/TWO-PI))
         (q/sin (mod (+ 0.01 (q/atan2 y x)) q/TWO-PI))])
      line))
   flow-field))

(defn update-state [state]
  (-> state
      (update :vehicles
              (fn [vehicles]
                (map
                 (comp (partial follow (:flow-field state)) move-through update-vehicle)
                 vehicles)))
      (update :flow-field rotate-arrows)))

(defn draw-vehicle [{:keys [r velocity]
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

(defn draw-arrow [x y rotate]
  (let [x (* 20 x)
        y (* 20 y)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate (+ q/HALF-PI (apply q/atan2 (reverse rotate))))
    (q/stroke-weight 2)
    (q/line 5 1 2 3)
    (q/line 5 1 8 3)
    (q/line 5 1 5 9)
    (q/pop-matrix)))

(defn draw-flow-field [field]
  (doall
   (for [x (range 35)]
     (doall
      (for [y (range 25)]
        (do
          (draw-arrow x y (nth (nth field x) y))))))))

(defn draw [{:keys [flow-field vehicles]}]
  (q/clear)
  (q/stroke 127 0 127)
  (draw-flow-field flow-field)
  (doall
   (map draw-vehicle vehicles)))

(q/defsketch flow-field-change
  :title "flow-field-change"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :update update-state
  :features [:no-bind-output]
  :size [700 500])

(ns nature-of-code.06-autonomous-agents.exercise-06-10
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]
            [nature-of-code.mover :as m]))

(defn state []
  {:path {:r 20
          :lines ()
          :points (list [350 0]
                        [400 (/ (q/height) 3)]
                        [300 (/ (q/height) 3)]
                        [450 (/ (q/height) 2)]
                        [350 (/ (q/height) 1)])}
   :vehicle {:acceleration [0 0]
             :velocity [0 0]
             :location [150 150]
             :r 10
             :maxspeed 4.0
             :maxforce 0.1
             :prediction [0 0]
             :normal-point [0 0]}})

(defn setup []
  (state))

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn seek [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/mult (v/normalize (v/sub target location)) maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn get-normal-point [p a b]
  (let [ap (v/sub p a)
        ab (v/sub b a)
        ab-normalized (v/normalize ab)]
    (v/add a (v/mult ab-normalized (v/dot-product ap ab-normalized)))))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn update-lines [{:keys [location velocity] :as vehicle} {:keys [points] :as path}]
  (assoc path :lines
         (map-indexed
          (fn [idx point]
            (let [p            (v/add location  (v/mult (v/normalize velocity) 25))
                  a            point
                  b            (get (into [] points) (inc idx))
                  [nx ny]      (get-normal-point p a b)
                  normal-point (if (or (< nx (first a)) (> nx (first b))) b [nx ny])
                  distance     (v/mag (v/sub normal-point p))]
              {:normal-point normal-point :start a :end b :distance distance}))
          (butlast points))))

(defn follow-path [{:keys [points r]} {:keys [location velocity] :as vehicle}]
  (let [{:keys [normal-point distance line]} (reduce
                                         (fn [current point]
                                           (if (< (:distance point) (:distance current)) point current))
                                         {:distance 10000000 :normal-point [0 0]}
                                         (map-indexed
                                          (fn [idx point]
                                            (let [p            (v/add location  (v/mult (v/normalize velocity) 25))
                                                  a            point
                                                  b            (get (into [] points) (inc idx))
                                                  [nx ny]      (get-normal-point p a b)
                                                  normal-point (if (or (< nx (first a)) (> nx (first b))) b [nx ny])
                                                  distance (v/mag (v/sub normal-point p))]
                                              {:distance distance :normal-point normal-point :line b}))
                                          (butlast points)))
        target (v/add (v/mult (v/normalize line) 25) normal-point)]
    (if (> distance r)
      (seek vehicle target)
      vehicle)))

(defn update-state [{:keys [vehicle path] :as current-state}]
  (let []
    (-> (merge current-state)
        (update :path (partial update-lines vehicle))
        (update :vehicle (comp
                          update-vehicle
                          (partial follow-path path))))))

(defn draw-path [r point]
  (q/fill 255)
  (q/stroke-weight r)
  (q/stroke 127 127 127 127)
  (apply q/vertex point))

(defn draw-vehicle [{:keys [r velocity] [x y] :location}]
  (let [theta (+ (q/atan2 (second velocity) (first velocity)) q/HALF-PI)]
    (q/fill 175)
    (q/stroke 0)
    (q/stroke-weight 0)
    (q/push-matrix)
    (q/translate x y)
    (q/rotate theta)
    (q/begin-shape)
    (q/vertex 0 (- r))
    (q/vertex (- r) (* r 2))
    (q/vertex r (* r 2))
    (q/end-shape :close)
    (q/pop-matrix)))

(defn draw-line [{[xl yl] :location} {[x y] :normal-point}]
  (q/stroke-weight 1)
  (q/line xl yl x y)
  (q/ellipse x y 10 10))

(defn draw [{:keys [path vehicle prediction normal-point]}]
  (q/background 255)
  (q/begin-shape)
  (doall (map (partial draw-path (:r path)) (:points path)))
  (q/end-shape)
  (doall (map (partial draw-line vehicle) (:lines path)))
  (let [{:keys [location velocity]} vehicle
        [x y] (v/add location (v/mult (v/normalize velocity) 25))]
    (q/line (first location) (second location) x y)
    (q/ellipse x y 5 5))
  (draw-vehicle vehicle))

(defn run []
  (q/defsketch path-following
    :title "path-following"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))



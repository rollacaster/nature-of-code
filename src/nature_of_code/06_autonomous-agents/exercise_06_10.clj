(ns nature-of-code.06-autonomous-agents.exercise-06-10
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [quil.sketch :as s]
            [nature-of-code.vector :as v]
            [nature-of-code.mover :as m]))

(defn state []
  {:path {:r 20
          :points (list [350 0]
                        [400 (/ (q/height) 4)]
                        [450 (/ (q/height) 2)]
                        [350 (/ (q/height) 1)])}
   :vehicle {:acceleration [0 0]
             :velocity [0 0]
             :location [(/ (q/width) 2) (/ (q/height) 2)]
             :r 10
             :maxspeed 4.0
             :maxforce 0.1
             :prediction [0 0]
             :normal-point [0 0]
             :target [0 0]
             :on-track false}})

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

(defn follow-path [{:keys [points r]} {:keys [location velocity prediction] :as vehicle}]
  (let [world-record 10000000
        distances (map-indexed
                   (fn [idx point]
                     (let [prediction   (v/add location  (v/mult (v/normalize velocity) 25))
                           a            point
                           b            (get (into [] points) (inc idx))
                           [nx ny]      (get-normal-point prediction a b)
                           normal-point (if (or
                                             (or (< nx (first a)) (> nx (first b)))
                                             (or (< ny (second a)) (> ny (second b)))) b [nx ny])
                           distance (v/mag (v/sub prediction normal-point))]
                       {:distance distance :normal-point normal-point :line (v/normalize (v/sub b a))}))
                   (butlast points))
        {:keys [normal-point distance line]} (reduce
                                              (fn [{:keys [distance]
                                                    [nx ny] :normal-point
                                                    :as current} point]
                                                (if (< (:distance point) distance) point current))
                                              {:distance world-record :normal-point [0 0]}
                                              distances)
        target (v/add (v/mult (v/mult line 25) -1) normal-point)]
    (let [v (-> vehicle
                (assoc :prediction (v/add location  (v/mult (v/normalize velocity) 25)))
                (assoc :normal-point normal-point)
                (assoc :target target)
                (assoc :on-track (> distance r)))]
      (if (> distance r)
        (seek v target)
        v))))

(defn update-state [{:keys [vehicle path] :as current-state}]
  (let []
    (-> (merge current-state)
        (update :vehicle (comp
                          update-vehicle
                          m/move-through
                          (partial follow-path path))))))

(defn draw-path [{:keys [points r]}]
  (q/push-matrix)
  (q/stroke 99)
  (q/stroke-weight (* r 2))
  (q/no-fill)
  (q/begin-shape)
  (doseq [[x y] points]
    (q/vertex x y))
  (q/end-shape)
  (q/stroke 255)
  (q/stroke-weight 1)
  (q/no-fill)
  (q/begin-shape)
  (doseq [[x y] points]
    (q/vertex x y))
  (q/end-shape)
  (q/pop-matrix))

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

(defn draw-debug [{[x y] :location [px py] :prediction [nx ny] :normal-point
                   [tx ty] :target :keys [on-track]}]
  (q/stroke 255)
  (q/fill 200)
  (q/line x y px py)
  (q/ellipse px py 4 4)
  (q/ellipse nx ny 4 4)
  (q/line px py nx ny)
  (if on-track (q/fill 255 0 0) (q/fill 0 0 0))
  (q/no-stroke)
  (q/ellipse tx ty 8 8))

(defn draw [{:keys [path vehicle]}]
  (q/background 64)
  (draw-path path)
  (draw-debug vehicle)
  (draw-vehicle vehicle))

(defn start []
  (q/defsketch path-following
    :display 1
    :title "path-following"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))



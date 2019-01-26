(ns nature-of-code.05-physics-libraries.exercise-05-07
  (:require [clojure.pprint :refer [print-table]]
            [clojure.reflect :as r]
            [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 100.0]))
(def car (create-car world 100 200))
(def wheel1 (create-wheel world 130 210))
(def wheel2 (create-wheel world 70 210))
(def street (create-road world))
(def wheel-joint
  (b/joint! {:type :revolute
             :body-a car
             :body-b wheel1
             :world-anchor (b/center wheel1)
             :max-motor-torque 1000000.0
             :motor-speed (* 1000 q/TWO-PI)}))
(def wheel-joint2
  (b/joint! {:type :revolute
             :body-a car
             :body-b wheel2
             :world-anchor (b/center wheel2)
             :max-motor-torque 1000000.0
             :motor-speed (* 1000 q/TWO-PI)}))
(.enableMotor wheel-joint true)
(.enableMotor wheel-joint2 true)

(defn create-box [x y]
  (b/body! world {:position [x y]} {:shape (b/box 8 8)}))

(defn draw-body [body]
  (let [coords (b/world-coords (b/fixture-of body))]
    (q/begin-shape)
    (doseq [v coords]
      (apply q/vertex v))
    (q/end-shape :close)))

(defn setup []
  )

(defn create-car [world x y]
  (b/body! world {:position [x y]}
           {:shape (b/box 50 10)}
           {:shape (b/polygon [[-50 -10] [0 -30] [50 -10]])}))

(defn create-wheel [world x y]
  (b/body! world {:position [x y]}
           {:shape (b/circle 10)}))

(defn create-road [world]
  (b/body! world
           {:type :static}
           {:shape (b/edge-chain
                    (map (fn [x] [x
                                  (+ 220 (* 20 (q/sin (/ x 64))))])
                         (range 640)))}))

(defn draw-car [body]
  (let [[chassis roof] (b/fixtureseq body)]
    (q/begin-shape)
    (doseq [[x y] (b/world-coords chassis)]
      (q/vertex x y))
    (q/end-shape :close)
    (q/begin-shape)
    (doseq [[x y] (b/world-coords roof)]
      (q/vertex x y))
    (q/end-shape :close)
    (q/begin-shape)
    (doseq [[x y] (b/world-coords (b/fixture-of wheel1))]
      (q/vertex x y))
    (q/end-shape :close)
    (q/begin-shape)
    (doseq [[x y] (b/world-coords (b/fixture-of wheel2))]
      (q/vertex x y))
    (q/end-shape :close)))

(defn draw-street [body]
  (q/stroke-weight 1)
  (q/stroke 0)
  (q/no-fill)   
  (q/begin-shape)
  (doseq [[x y] (b/world-coords (b/fixture-of body))]
    (q/vertex x y))
  (q/end-shape))

(defn draw []
  (b/step! world (/ 1.0 60.0))
  (q/clear)
  (q/background 127)
  (q/fill 255)
  (draw-car car)
  (draw-street street)
  (q/fill 0))

(q/defsketch revolute-joint
  :title "revolute-joint"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [640 360])

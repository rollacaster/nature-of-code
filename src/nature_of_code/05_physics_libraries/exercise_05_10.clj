(ns nature-of-code.05-physics-libraries.exercise-05-10
  (:require [nature-of-code.vector :as v]
            [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(do
  (def world (b/new-world [0 10.0]))
  (def box1 (b/body! world {:position [150 100]} {:shape (b/box 10 10)}))
  (def box2 (b/body! world {:position [200 100]} {:shape (b/box 10 10)}))
  (def wall1 (b/body! world {:position [0 0] :type :static} {:shape (b/box 10 500)}))
  (def wall2 (b/body! world {:position [0 500] :type :static} {:shape (b/box 700 10)}))
  (def wall3 (b/body! world {:position [700 0] :type :static} {:shape (b/box 10 500)})))

(defn setup [])

(defn draw-body [body]
  (q/begin-shape)
  (doseq [[x y] (b/world-coords (b/fixture-of body))]
    (q/vertex x y))
  (q/end-shape :close))

(defn attract [b1 b2]
  (let [G 0.4
        pos (b/center b1)
        moverPos (b/center b2)
        force (v/sub pos moverPos)
        distance (q/constrain (v/mag force) 1.0 5.0)
        strength (/ (* G (b/mass b2)) (* distance distance))]
    (v/mult (v/normalize force) strength)))

(defn draw []
  (q/clear)
  (q/background 255)
  (b/step! world (/ 1.0 6.0))
  (b/apply-force! box1 (attract box1 box2) (b/center box1))
  (b/apply-force! box2 (attract box2 box1) (b/center box2))
  (draw-body box1)
  (draw-body box2))



(defn run []
  (q/defsketch box2d-forces
    :title "box2d-forces"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

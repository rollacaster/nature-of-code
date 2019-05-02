(ns nature-of-code.05-physics-libraries.exercise-05-08
  (:require [nature-of-code.vector :as v]
            [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn create-box [world x y]
  (b/body! world {:position [x y] :type :kinematic} {:shape (b/box 8 8) :restitution 0.5}))

(do
  (def world (b/new-world [0 100.0]))
  (def box (create-box world 100 100))
  (def walls [(b/body! world {:position [0 0] :type :static} {:shape (b/box 700 10)})
              (b/body! world {:position [0 0] :type :static} {:shape (b/box 10 700)})
              (b/body! world {:position [0 500] :type :static} {:shape (b/box 700 10)})
              (b/body! world {:position [700 500] :type :static} {:shape (b/box 10 700)})]))

(defn draw-body [body]
  (q/begin-shape)
  (doseq [[x y] (b/world-coords (b/fixture-of body))]
    (q/vertex x y))
  (q/end-shape :close))

(defn draw-spring [mouse-joint]
  (let [[x1 y1] (b/anchor-a mouse-joint)
        [x2 y2] (b/anchor-b mouse-joint)]
    (q/line x1 y1 x2 y2)))

(defn draw []
  (q/clear)
  (q/background 200)
  (b/step! world (/ 1.0 60.0))
  #_(b/linear-velocity! box
                    (v/sub [(q/mouse-x) (q/mouse-y)] (b/center (b/fixture-of box))))
  (draw-body box)
  (doall (map draw-body walls)))

(defn key-pressed []
  (cond
    (= (q/key-code) 38) (b/linear-velocity! box [0 -10])
    (= (q/key-code) 40) (b/linear-velocity! box [0 10])
    (= (q/key-code) 37) (b/linear-velocity! box [-10 0])
    (= (q/key-code) 39) (b/linear-velocity! box [10 0])))

(defn setup [])

(defn run []
  (q/defsketch mouse-joint-box
    :title "mouse-joint"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :draw draw
    :setup setup
    :features [:no-bind-output]
    :size [700 500]
    :key-pressed key-pressed))

(ns nature-of-code.05-physics-libraries.exercise-05-01
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 100]))

(defn setup []
  ())

(defn create-box [world x y]
  (let [body (b/body! world {:position [x y]} {:shape (b/box 8 8) :restitution 0.5})]
    {:body body :w 16 :h 16}))

(defn update-state [boxes]
  (if (q/mouse-pressed?)
    (conj boxes (create-box world (q/mouse-x) (q/mouse-y)))
    boxes))

(defn draw-box [{:keys [body w h]}]
  (let [[[x y]] (b/world-coords (b/fixture-of body))
        angle (b/angle body)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/fill 175)
    (q/stroke 0)
    (q/rect-mode :center)
    (q/rect 0 0 w h)
    (q/pop-matrix)))

(defn draw [boxes]
  (q/clear)
  (q/background 127)
  (b/step! world (/ 1.0 60.0))
  (doseq [box boxes]
    (draw-box box)))

(defn run []
  (q/defsketch physics
    :title "physics"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))

(ns nature-of-code.08-fractals.exercise-08-01
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn draw-circle [x y r]
  (q/ellipse x y r r)
  (q/ellipse (+ x (/ r 2)) y (/ r 2) (/ r 2))
  (q/ellipse (- x (/ r 2)) y (/ r 2) (/ r 2)))

(defn cantor [x y l]
  (when (>= l 1)
    (draw-circle x y l)
    #_(q/line x y (+ x l) y)
    (let [y2 (+ y 20)]
      (cantor x y2 (/ l 3))
      (cantor (+ x (* l (/ 2 3))) y2 (/ l 3)))))

(defn draw [state]
  (q/clear)
  (q/background 255)
  (q/stroke-weight 5)
  (cantor 20 20 (/ (q/width) 1)))

(defn update-state [state]
)

(defn setup []
  )

(defn run []
  (q/defsketch own-cantor
    :title "own-cantor"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :display 1
    :size [700 500]))

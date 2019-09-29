(ns nature-of-code.08-fractals.exercise-08-05
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn setup []
  0)

(defn update-state [state]
  state)

(defn compute-triangles [v1 v2 v3]
  (let [[x1 y1] (v/mult (v/add v1 v2) 0.5)
        [x2 y2] (v/mult (v/add v1 v3) 0.5)
        [x3 y3] (v/mult (v/add v3 v2) 0.5)]
    [[v1 [x1 y1] [x2 y2]]
     [[x1 y1] v2 [x3 y3]]
     [[x2 y2] [x3 y3] v3]]))

(defn do-sierpinski [init-triangle step]
  (if (= step 0)
    init-triangle
    (mapcat
     #(do-sierpinski % (- step 1))
     (apply compute-triangles init-triangle))))

(defn sierpinski [init-triangle step]
  (partition 3 (do-sierpinski init-triangle step)))

(defn draw [state]
  (q/background 200)
  (q/fill 0)
  (q/translate 150 50)
  (let [step (q/round (q/map-range (q/mouse-x) 0 (q/width) 0 6))
        [x1 y1] [100 280]
        [x2 y2] [200 120]
        [x3 y3] [300 280]]
    (doall
     (map
      (fn [[[x1 y1] [x2 y2] [x3 y3]]]
        (q/triangle x1 y1 x2 y2 x3 y3))
      (sierpinski [[x1 y1] [x2 y2] [x3 y3]] step)))))

(defn run []
  (q/defsketch sierpinski-sketch
    :title "sierpinski"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :features [:no-bind-output]
    :size [700 500]))


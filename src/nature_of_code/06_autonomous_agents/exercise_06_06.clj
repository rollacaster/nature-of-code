(ns nature-of-code.06-autonomous-agents.exercise-06-06
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]))

(defn setup []
  (def flow-field
    (doall
     (for [x (range 35)]
       (doall
        (for [y (range 25)]
          (+ q/HALF-PI (apply q/atan2 (reverse (v/sub [(/ (q/width) 2) (/ (q/height) 2)] [(* 20 x) (* 20 y)]))))))))))

(defn draw-arrow [x y rotate]
  (let [x (* 20 x)
        y (* 20 y)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate rotate)
    (q/stroke-weight 2)
    (q/line 5 1 2 3)
    (q/line 5 1 8 3)
    (q/line 5 1 5 9)
    (q/pop-matrix)))

(defn draw []
  (q/clear)
  (q/stroke 127 0 127)
  (doall
   (for [x (range 35)]
     (doall
      (for [y (range 25)]
        (draw-arrow x y (nth (nth flow-field x) y)))))))

(defn run []
  (q/defsketch flow-field-center
    :title "flow-field-center"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

(ns nature-of-code.08-fractals.exercise-08-12
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn compute-turtle [[start end] len angle]
  (let [a start
        b (v/add start (v/mult (v/normalize end) len))
        c end
        d (v/add c (v/rotate (v/mult (v/normalize c) len) (* 2 angle)))
        e (v/add d (v/rotate (v/mult (v/normalize d) len) (/ angle 2)))
        f (v/add e (v/rotate (v/mult (v/normalize e) len) (- angle)))
        g (v/add c (v/rotate (v/mult (v/normalize c) len) (- angle)))
        h (v/add g (v/rotate (v/mult (v/normalize g) len) (/ angle 2)))
        i (v/add h (v/rotate (v/mult (v/normalize h) len) angle))]
    [[a b]
     [b c]
     [c d]
     [d e]
     [e f]
     [c g]
     [g h]
     [h i]]))

(defn setup []
  (let [len (/ 700 4)
        angle (q/radians 25)]
    {:length len
     :angle angle
     :lines [[[0 0] [0 (+ (- len) 0)]]]}))

(defn draw [{:keys [lines]}]
  (q/clear)
  (q/color-mode :hsb)
  (q/background 255)
  (q/translate (/ (q/width) 2) (q/height))
  (q/fill 0)
  (let [line-groups (partition-all 8 lines)]
    (doseq [[idx lines-seq] (map-indexed #(vector %1 %2) line-groups)]
      (q/stroke (* idx (/ 255 (count line-groups))) 255 255)
      (doseq [[[x1 y1] [x2 y2]] lines-seq]
        (q/stroke-weight 10)
        (q/stroke-weight 1)
        (q/line x1 y1 x2 y2)))))

(defn mouse-pressed [state ev]
  (let [len (/ (:length state) 2)]
    (-> state
        (update :lines (fn [lines] (mapcat #(compute-turtle % len (:angle state)) lines)))
        (assoc :length len))))

(defn run []
  (q/defsketch l-system-sketch
    :title "l-system"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :display 1
    :features [:no-bind-output]
    :size [700 500]))

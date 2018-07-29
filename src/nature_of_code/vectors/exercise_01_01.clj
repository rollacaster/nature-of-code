(ns nature-of-code.vectors.exercise-01-01
  (:require [quil.core :as q]))

(defn add [v1 v2]
  (hash-map :x (+ (:x v1) (:x v2))
            :y (+ (:y v1) (:y v2))))

(defn setup []
  (def location (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [choice (q/random 1)
        velocity (cond
                   (< choice 0.500) (hash-map :x (if (> (:x @location) (q/mouse-x)) -1 1)
                                              :y (if (> (:y @location) (q/mouse-y)) -1 1))
                   (< choice 0.625) {:x 1 :y 0}
                   (< choice 0.750) {:x -1 :y 0}
                   (< choice 0.875) {:x 0 :y 1}
                   :else {:x 0 :y -1})]
    (let [{:keys [x y]} (swap! location #(add % velocity))]
      (q/point x y))))

(q/defsketch pvector
  :title "pvector"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

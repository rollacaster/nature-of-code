(ns nature-of-code.vectors.01-01-example
  (:require [quil.core :as q]))

(def location (atom (hash-map :x 100 :y 100 :z 0)))
(def velocity (atom (hash-map :x 2.5 :y 5 :z 0)))

(defn add [v1 v2]
  (hash-map :x (+ (:x v1) (:x v2))
            :y (+ (:y v1) (:y v2))
            :z (+ (:z v1) (:z v2))))

(defn setup []
  (q/background 255))

(defn draw []
  (q/background 255)
  (q/stroke 0)
  (q/fill 175)
  (let [location (swap! location #(add % @velocity))
        velocity (swap! velocity (fn [{:keys [x y z]}]
                                   (hash-map
                                    :x (if (or (> (:x location) (q/width)) (< (:x location) 0))
                                         (* x -1) x)
                                    :y (if (or (> (:y location) (q/height)) (< (:y location) 0))
                                         (* y -1) y)
                                    :z (if (or (> (:z location) 100) (< (:y location) 0))
                                         (* y -1) y))))]
    (q/translate (:x location) (:y location) (:z location))
    (q/sphere  16)))

(q/defsketch bouncingBall
  :title "bouncingBall"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500]
  :renderer :opengl)

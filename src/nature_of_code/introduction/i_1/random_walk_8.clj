(ns nature-of-code.introduction.i-1.random-walk-8
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [stepx (- (rand-int 3) 1)
        stepy (- (rand-int 3) 1)]
    (swap! walker (fn [{:keys [x y]}] {:x (+ x stepx) :y (+ y stepy)})))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

#_(q/defsketch random-walk-8
  :title "Random Walk 8 Directions"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

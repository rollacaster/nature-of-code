(ns nature-of-code.introduction.i-1.random-walk-right
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [choice (q/random 1)]
    (cond (< choice 0.4) (swap! walker #(update % :x inc))
          (< choice 0.6) (swap! walker #(update % :x dec))
          (< choice 0.8) (swap! walker #(update % :y inc))
          :else (swap! walker #(update % :y dec))))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

#_(q/defsketch random-walk-down-right
  :title "Random Walk Tends to walk down right"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

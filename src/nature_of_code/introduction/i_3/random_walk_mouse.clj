(ns nature-of-code.introduction.i-3.random-walk-mouse
    (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [choice (q/random 1)]
    (cond (< choice 0.500)
          (swap! walker #(-> %
                             (update :x (fn [x] (if (> x (q/mouse-x)) (dec x) (inc x))))
                             (update :y (fn [y] (if (> y (q/mouse-y)) (dec y) (inc y))))))
          (< choice 0.625) (swap! walker #(update % :x inc))
          (< choice 0.750) (swap! walker #(update % :x dec))
          (< choice 0.875) (swap! walker #(update % :y inc))
          :else (swap! walker #(update % :y dec))))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

(q/defsketch random-walk-down-right
  :title "Random Walk Tends to walk down right"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

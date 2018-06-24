(ns nature-of-code.introduction.random-walk
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [choice (rand-int 4)]
    (cond (= choice 0) (swap! walker #(update % :x inc))
          (= choice 1) (swap! walker #(update % :x dec))
          (= choice 2) (swap! walker #(update % :y inc))
          :else (swap! walker #(update % :y dec))))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

(q/defsketch random-walk
  :title "Random Walk"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

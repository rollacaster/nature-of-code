(ns nature-of-code.introduction.i-4.gaussian-random-walk
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn gaussian-inc [x]
  (+ (+ (* (q/random-gaussian) 5) (/ (q/width) 2)) x))

(defn gaussian-dec [x]
  (- (+ (* (q/random-gaussian) 5) (/ (q/width) 2)) x))

(defn draw []
  (q/stroke 0)
  (let [choice (rand-int 4)]
    (cond (= choice 0) (swap! walker #(update % :x gaussian-inc))
          (= choice 1) (swap! walker #(update % :x gaussian-dec))
          (= choice 2) (swap! walker #(update % :y gaussian-inc))
          :else (swap! walker #(update % :y gaussian-dec))))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

(q/defsketch random-walk-gaus
  :title "Random Walk Gaussian"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

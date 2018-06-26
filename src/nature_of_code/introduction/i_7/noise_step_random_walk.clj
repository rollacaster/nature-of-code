(ns nature-of-code.introduction.i-7.noise-step-random-walk
    (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (def tx (atom 0))
  (def ty (atom 10000))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [tx (swap! tx #(+ 0.01 %))
        choice (rand-int 4)
        step (q/map-range (q/noise tx) 0 1 0 10)
        {:keys [x y]}
        (cond (= choice 0) (swap! walker #(update % :x (fn [x] (+ x step))))
              (= choice 1) (swap! walker #(update % :x (fn [x] (- x step))))
              (= choice 2) (swap! walker #(update % :y (fn [x] (+ x step))))
              :else (swap! walker #(update % :y (fn [x] (- x step)))))]
    (q/point x y)))

#_(q/defsketch random-walk-step-noise
  :title "Random Walk Noise Step"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

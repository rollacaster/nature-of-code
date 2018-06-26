(ns nature-of-code.introduction.i-5.noise-random-walk
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (def tx (atom 0))
  (def ty (atom 10000))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [tx (swap! tx #(+ 0.01 %))
        ty (swap! ty #(+ 0.01 %))
        {:keys [x y]} (swap! walker (fn [_]
                                      {:x (q/map-range (q/noise tx) 0 1 0 (q/width))
                                       :y (q/map-range (q/noise ty) 0 1 0 (q/height))}))]
    (q/point x y)))

#_(q/defsketch random-walk-Noise
  :title "Random Walk Noise"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

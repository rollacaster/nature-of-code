(ns nature-of-code.introduction.i-6.random-walk-monte-carlo
  (:require [quil.core :as q]))

(defn setup []
  (def walker (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (q/background 255))

(defn monte-carlo []
  (loop []
      (let [r1 (q/random 1)
            probability r1
            r2 (q/random 1)]
        (if (< r2 probability) r1 (recur)))))

(defn draw []
  (q/stroke 0)
  (let [stepsize (q/pow (q/random 0 10) 2)
        stepx (q/random (-' stepsize) stepsize)
        stepy (q/random (-' stepsize) stepsize)]
    (swap! walker (fn [{:keys [x y]}] {:x (+ x stepx) :y (+ y stepy)})))
  (let [{:keys [x y]} @walker]
    (q/point x y)))

#_(q/defsketch random-walk-monte-carlo
  :title "Random Walk Monte Carlo"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

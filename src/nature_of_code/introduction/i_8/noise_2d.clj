(ns nature-of-code.introduction.i-8.noise-2d
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  (let [xoff (atom 0.0)]
    (doseq [x (range 0 (q/width))]
      (let [yoff (atom 0.0)]
        (doseq [y (range 0 (q/height))]
          (q/stroke (q/map-range (q/noise @xoff @yoff) 0 1 0 255))
          (q/point x y)
          (swap! yoff #(+ 0.01 %)))
        (swap! xoff #(+ 0.01 %))))))

(defn draw [])

#_(q/defsketch noise-2d
  :title "noise-2d"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])




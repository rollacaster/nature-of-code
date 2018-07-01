(ns nature-of-code.introduction.i-8.noise-color
  (:require [quil.core :as q]))

(defn setup []
)

(defn draw []
  (q/clear)
  (q/color-mode :hsb)
  (q/noise-detail 2)
  (let [xoff (atom 0)]
    (doseq [x (range (q/width))]
      (doseq [y (range (q/height))]
        (swap! xoff #(+ 0.01 %))
        (q/stroke (q/color (q/map-range (q/noise @xoff) 0 1 0 360)
                           100
                           100))
        (q/point x y)))))

#_(q/defsketch noise-color
  :title "noise-color"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

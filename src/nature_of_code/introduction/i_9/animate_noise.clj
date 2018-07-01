(ns nature-of-code.introduction.i-9.animate-noise
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255))

(def zoff (atom 0))

(defn draw []
  (q/clear)
  (q/background 255)
  (let [xoff (atom 0.0)]
    (doseq [x (range 0 (q/width))]
      (let [yoff (atom 0.0)]
        (doseq [y (range 0 (q/height))]
          (q/stroke (q/map-range (q/noise @xoff @yoff @zoff) 0 1 0 255))
          (q/point x y)
          (swap! yoff #(+ 0.01 %)))
        (swap! xoff #(+ 0.01 %)))))
  (swap! zoff #(+ 0.01 %)))

#_(q/defsketch animate-noise
  :title "animate-noise"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

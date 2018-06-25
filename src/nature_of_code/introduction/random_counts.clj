(ns nature-of-code.introduction.random-counts
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  (def counts (atom (range 20))))

(defn draw []
  (swap! counts (fn [counts]
                  (let [index (rand-int (count counts))]
                    (map-indexed (fn [i count] (if (= i index) (+ 1 count) count)) counts))))

  (q/stroke 0)
  (q/fill 175)
  (let [counts @counts
        w (/ (q/width) (count counts))]
    (doseq [[x y w h] (map-indexed
                       (fn [i count]
                         [(* i w) (- (q/height) count) (- w 1) count])
                       counts)]
      (q/rect x y w h))))

(q/defsketch random-counts
  :title "random-counts"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])



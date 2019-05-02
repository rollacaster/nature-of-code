(ns nature-of-code.02-forces.exercise-02-02
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]))

(def movers (atom []))

(loop [times 20]
  (when (> times 0)
    (swap! movers #(conj % {:mass (+ 1 (rand-int 20))
                            :location [0 0]
                            :velocity [0 0]
                            :acceleration [0 0]}))
    (recur (dec times))))

(defn keep-inside [{[x y] :location} width height]
  (vector
   (if (> x width) (- width x)
       (if (< x 0) (* -1 x) 0))
   (if (> y height) (- height y)
       (if (< y 0) (* -1 y) 0))))

(defn update-mover [height width mover]
  (let [wind [0.1 0] gravity [0 1]]
    (-> mover
        (m/apply-force wind)
        (m/apply-force gravity)
        (m/apply-force (keep-inside mover width height))
        (m/compute-position))))

(defn draw []
  (q/clear)
  (q/background 255)
  (q/fill 255 0 255)
  (doseq [{:keys [mass] [x y] :location}
          (swap! movers #(map (partial update-mover (q/height) (q/width)) %))]
    (q/ellipse x y mass mass)))

(defn run []
  (q/defsketch bouncingball
    :title "bouncingball"
    :settings #(q/smooth 2)
    :draw draw
    :size [500 500]))

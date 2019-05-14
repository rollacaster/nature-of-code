(ns nature-of-code.02-forces.exercise-02-05
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))

(defn setup []
  (map
   (fn [i] {:mass (+ 1 (rand-int 50))
            :location [(* i 50) (* i 50)]
            :velocity [0 0]
            :acceleration [0 0]})
   (range 10)))

(defn compute-position [{:keys [acceleration velocity location] :as mover}]
  (let [velocity (v/add acceleration velocity)
        location (v/add velocity location)]
    (-> mover
        (assoc :acceleration [0 0])
        (assoc :velocity velocity)
        (assoc :location location))))

(defn liquid [x y w h c]
  {:location [x y]
   :size [w h]
   :c c})

(defn gravity [{:keys [mass]}]
  (vector 0 (* 0.1 mass)))

(defn drag [mover liquid]
  (let [{[x y] :location :keys [velocity]} mover
        {:keys [c location]} liquid
        speed (v/mag velocity)]
    (if (> y (second location))
      (v/mult (v/normalize (v/mult velocity -1))
              (* c speed speed))
      [0 0])))

(defn update-mover [mover]
  (-> mover
      (m/apply-force (gravity mover))
      (m/apply-force (drag mover (liquid 0 (/ (q/height) 2) (q/width) (/ (q/height) 2) 1)))
      compute-position
      m/keep-inside))

(defn update-state [movers]
  (map update-mover movers))

(defn draw [movers]
  (q/clear)
  (q/background 255)
  (q/fill 50 100 255)
  (q/rect 0 (/ (q/height) 2) (q/width) (/ (q/height) 2))
  (q/fill 255 0 255)
  (doseq [{:keys [mass velocity] [x y] :location} movers]
    (q/ellipse x y mass mass)))

(defn run []
  (q/defsketch fluid-resistance
    :title "fluid-resistance"
    :settings #(q/smooth 2)
    :setup setup
    :update update-state
    :draw draw
    :size [500 500]
    :middleware [md/fun-mode]))




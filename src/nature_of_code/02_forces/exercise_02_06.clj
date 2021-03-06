(ns nature-of-code.02-forces.exercise-02-06
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))

(defn setup []
  {:movers (map
            (fn [i]
              {:mass (+ 1 (rand-int 50))
               :location [(* i 50) 0]
               :velocity [0 0]
               :acceleration [0 0]})
            (range 50))
   :liquid {:location [0 (/ (q/height) 2)]
            :size [(q/width) (/ (q/height) 2)]
            :c 0.1}})

(defn draw [{:keys [movers]}]
  (q/clear)
  (q/background 255)
  (q/fill 50 100 255)
  (q/rect 0 (/ (q/height) 2) (q/width) (/ (q/height) 2))
  (q/fill 255 0 255)
  (doseq [{:keys [mass] [x y] :location} movers]
    (q/rect x (-  y mass) mass mass)))

(defn compute-position [{:keys [acceleration velocity location] :as mover}]
  (let [velocity (v/add acceleration velocity)
        location (v/add velocity location)]
    (-> mover
        (assoc :acceleration [0 0])
        (assoc :velocity velocity)
        (assoc :location location))))

(defn gravity [{:keys [mass]}]
  [0 (* 0.1 mass)])

(defn drag [{[x y] :location :keys [velocity mass]}
            {:keys [c location]}]
  (let [speed (v/mag velocity)
        drag-magnitude (* c speed speed)
        A mass]
    (if (> y (second location))
      (v/mult (v/mult (v/normalize (v/mult velocity -1)) drag-magnitude) A) 
      [0 0])))

(defn update-mover [liquid mover]
  (-> mover
      (m/apply-force (gravity mover))
      (m/apply-force (drag mover liquid))
      compute-position
      m/keep-inside))

(defn update-state [state]
  (update state :movers #(map (partial update-mover (:liquid state)) %)))

(defn run []
  (q/defsketch surface-area
    :title "surface-area"
    :settings #(q/smooth 2)
    :setup setup
    :draw draw
    :update update-state
    :size [500 500]
    :middleware [md/fun-mode]))

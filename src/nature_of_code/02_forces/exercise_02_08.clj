(ns nature-of-code.02-forces.exercise-02-08
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.vector :as v]
            [nature-of-code.mover :as m]))

(defn compute-position [{:keys [acceleration velocity location] :as mover}]
  (let [velocity (v/add acceleration velocity)
        location (v/add velocity location)]
    (-> mover
        (assoc :acceleration [0 0])
        (assoc :velocity velocity)
        (assoc :location location))))

(defn setup []
  {:movers (map (fn [x] {:mass (+ 1 (rand-int 50))
                         :location [(rand-int 500) (rand-int 500)]
                         :velocity [0 0]
                         :acceleration [0 0]})
                (range 0 10))
   :attractors (map (fn [x]
                      {:mass (rand-int 20)
                       :location [(rand-int 500) (rand-int 500)]})
                    (range 0 5))})

(defn attract [mover attractor]
  (let [{loc1 :location} attractor  
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain-float (v/mag vectorBetween) 5.0 25.0)
        G 0.4
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/normalize vectorBetween) strength)))

(defn update-mover [attractors mover]
  (compute-position
   (reduce #(m/apply-force mover (attract %1 %2)) mover attractors)))

(defn update-state [{:keys [attractors] :as state}]
  (update state :movers (partial map (partial update-mover attractors))))

(defn draw [{:keys [movers]}]
  (q/clear) 
  (q/background 255)
  (doseq [{:keys [mass] [x y] :location} movers]
    (q/ellipse x y mass mass)))

(defn run []
  (q/defsketch mover-attractor
    :title "mover-attractor"
    :settings #(q/smooth 2)
    :setup setup
    :update update-state
    :draw draw
    :size [500 500]
    :middleware [md/fun-mode]))

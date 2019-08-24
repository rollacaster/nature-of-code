(ns nature-of-code.02-forces.exercise-02-10
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.middleware :as md]))

(defn compute-position [{:keys [acceleration velocity location] :as mover}]
  (let [velocity (v/add acceleration velocity)
        location (v/add velocity location)]
    (-> mover
        (assoc :acceleration [0 0])
        (assoc :velocity velocity)
        (assoc :location location))))

(defn repulse [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain (v/mag vectorBetween) 5.0 25.0)
        G 0.0001
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/mult (v/normalize vectorBetween) strength) -1)))

(defn setup []
  {:movers (map (fn [x] {:mass (+ 10 (rand-int 50))
                         :location [(rand-int 500) (rand-int 500)]
                         :velocity [0 0]
                         :acceleration [0 0]})
                (range 0 10))})

(defn update-mover [movers mover]
  (let [mouse {:mass 100 :location [(q/mouse-x) (q/mouse-y)]}]
    (compute-position
     (m/apply-force
      (reduce #(m/apply-force %1 (repulse %1 %2)) mover movers)
      (m/attract mover mouse)))))

(defn update-state [state]
  (update state :movers (fn [movers] (map #(update-mover movers %) movers))))

(defn draw [{:keys [movers]}]
  (q/clear)
  (q/background 255)
  (doseq [{:keys [mass] [x y] :location} movers]
    (q/ellipse x y mass mass)))

(defn run []
  (q/defsketch repulse-mouse
    :title "repulse-mouse"
    :settings #(q/smooth 2)
    :draw draw
    :setup setup
    :update update-state
    :middleware [md/pause-on-error md/fun-mode]
    :display 1
    :size [700 500]))



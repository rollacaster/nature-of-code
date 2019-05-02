(ns nature-of-code.02-forces.exercise-02-09
  (:require [quil.core :as q]
            [nature-of-code.vector :as v]
            [nature-of-code.mover :as m]))

(def movers (atom (->> (range 0 10)
                       (map (fn [x] {:mass (+ 1 (rand-int 50))
                                     :location [(rand-int 500) (rand-int 500)]
                                     :velocity [0 0]
                                     :acceleration [0 0]})))))


(def attractors (atom (->> (range 0 5)
                           (map (fn [x] {:mass (rand-int 20)
                                         :location [(rand-int 500) (rand-int 500)]})))))

(defn attract [mover attractor]
  (let [{loc1 :location} attractor  
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain-float (v/mag vectorBetween) 5.0 25.0)
        G 0.4
        strength (/ (* distanceBetween distanceBetween) (* G (:mass attractor) (:mass mover)))]
    (v/mult (v/normalize vectorBetween) strength)))

(defn compute-movers []
  (doseq [{:keys [mass] [x y] :location}
          (swap! movers (fn [movers] (map (fn [mover]
                                            (m/compute-position (reduce (fn [mover attractor]
                                                                          (m/apply-force mover (attract mover attractor)))
                                                                        mover
                                                                        @attractors))) movers)))]
    (q/ellipse x y mass mass)))

(defn draw []
  (q/clear) 
  (q/background 255)
  (compute-movers)
  (doseq [{:keys [mass] [x y] :location} @attractors]
    (q/rect x y mass mass)))

(defn run []
  (q/defsketch mover-attractor
    :title "mover-attractor-own-law"
    :settings #(q/smooth 2)
    :draw draw
    :size [500 500]))

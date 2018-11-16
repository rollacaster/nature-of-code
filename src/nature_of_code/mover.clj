(ns nature-of-code.mover
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]))

(defn apply-force [mover force]
  (assoc mover
         :acceleration
         (v/add (:acceleration mover) (v/div force (:mass mover)))))

(defn compute-position [mover]
  (-> mover
      (update :velocity #(v/add (:acceleration mover) %))
      (update :location #(v/add % (v/add (:acceleration mover) (:velocity mover))))
      (assoc :acceleration [0 0])))

(defn check-edges [mover width height]
  (let [{:keys [location]} mover
        [x y] location
        mover (if (> x width)
                (-> mover
                    (assoc :location [width y])
                    (update :velocity (fn [[x y]] (vector (* -1 x) y))))
                (if (< x 0)
                  (-> mover
                      (update :velocity (fn [[x y]] (vector (* -1 x) y)))
                      (assoc :location [0 y]))
                  mover))]
    (if (> y height)
      (-> mover
          (update :velocity (fn [[x y]] (vector x (* -1 y))))
          (assoc :location [x height]))
      mover)))

(defn keep-inside [mover width height]
  (let [{:keys [location]} mover
        [x y] location
        mover (if (> x width)
                (assoc mover :location [width y])
                (if (< x 0)
                  (assoc mover :location [0 y])
                  mover))]
    (if (> y height)
      (assoc mover :location [x height])
      mover)))

(defn attract [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain-float (v/mag vectorBetween) 5.0 25.0)
        G 2
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/normalize vectorBetween) strength)))

(defn repulse [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain-float (v/mag vectorBetween) 5.0 25.0)
        G 0.1
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/mult (v/normalize vectorBetween) strength) -1)))
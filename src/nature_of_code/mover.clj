(ns nature-of-code.mover
  (:require [nature-of-code.vector :as v]
            [quil.core :as q]))

(defn create-mover [mass location]
  {:mass mass
   :location location
   :velocity [0.0 0.0]
   :acceleration [0.0 0.0]
   :a-velocity 0.0
   :a-acceleration 0.0
   :angle 0.0})

(defn apply-force [{:keys [mass] :as mover} force]
  (update mover :acceleration #(v/add % (v/div force mass))))

(defn round [val]
  (float
   (/ (q/round (* val 100)) 100)))

(defn pol-to-cart [[r phi]]
  [(* r (q/cos phi)) (* r (q/sin phi))])

(defn draw-stats [{:keys [acceleration velocity location angle a-velocity a-acceleration]
                   [x y] :location
                   [vx vy] :velocity}]
  (q/push-matrix)
  (q/rect-mode :corner)
  (q/translate [(* (q/width) 0.75) (* (q/height) 0.01)])
  (q/rect 0 0 (* (q/width) 0.2) 105)
  (q/fill 255 255 255)
  (q/with-translation [10 20]
    (q/text (str "Location " [(q/round x) (q/round y)]) 0 0)
    (q/text (str "Velocity " [(round vx) (round vy)]) 0 15)
    (q/text (str "Polar " (let [[r phi] (pol-to-cart [vx vy])]
                            [(round r) (round phi)])) 0 30)
    #_(q/text (str "Acceleration " acceleration) 0 30)
    (q/text (str "Angle " (q/degrees (q/atan2 vy vx))) 0 45)
    (q/text (str "A-Velocity " a-velocity) 0 60)
    #_(q/text (str "A-Acceleration " a-acceleration) 0 75))
  (q/pop-matrix))

(defn compute-position [{:keys [acceleration velocity location angle a-velocity a-acceleration] :as mover}]
  (let [new-velocity (v/add acceleration velocity)
        new-location (v/add new-velocity location)
        new-a-velocity (+ a-acceleration a-velocity)
        new-angle (+ new-a-velocity angle)]
    (-> mover
        (assoc :velocity new-velocity)
        (assoc :location new-location)
        (assoc :a-velocity new-a-velocity)
        (assoc :angle new-angle)
        (assoc :a-acceleration 0)
        (assoc :acceleration [0 0]))))

(defn check-edges [mover]
  (let [{:keys [location]} mover
        [x y] location
        mover (if (> x (q/width))
                (-> mover
                    (assoc :location [(q/width) y])
                    (update :velocity (fn [[x y]] (vector (* -1 x) y))))
                (if (< x 0)
                  (-> mover
                      (update :velocity (fn [[x y]] (vector (* -1 x) y)))
                      (assoc :location [0 y]))
                  mover))]
    (if (> y (q/height))
      (-> mover
          (update :velocity (fn [[x y]] (vector x (* -1 y))))
          (assoc :location [x (q/height)]))
      mover)))

(defn move-through [mover]
  (let [{:keys [location]} mover
        [x y] location]
    (cond (> x (q/width)) (-> mover (assoc :location [0 y]))
          (< x 0) (-> mover (assoc :location [(q/width) y]))
          (> y (q/height)) (assoc mover :location [x 0])
          (< y 0) (assoc mover :location [x (q/height)])
          :else mover)))

(defn keep-inside [mover]
  (let [{:keys [location]} mover
        [x y] location
        mover (if (> x (q/width))
                (assoc mover :location [(q/width) y])
                (if (< x 0)
                  (assoc mover :location [0 y])
                  mover))]
    (if (> y (q/height))
      (assoc mover :location [x (q/height)])
      mover)))

(defn move-to [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/map-range (v/mag vectorBetween) 0 2000 0 0.1)]
    (apply-force mover (v/mult vectorBetween distanceBetween))))

(defn attract [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain (v/mag vectorBetween) 5.0 25.0)
        G 0.4
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/normalize vectorBetween) strength)))

(defn repulse [mover attractor]
  (let [{loc1 :location} attractor
        {loc2 :location} mover
        vectorBetween (v/sub loc1 loc2)
        distanceBetween (q/constrain (v/mag vectorBetween) 5.0 25.0)
        G 0.1
        strength (/ (* G (:mass attractor) (:mass mover)) (* distanceBetween distanceBetween))]
    (v/mult (v/mult (v/normalize vectorBetween) strength) -1)))

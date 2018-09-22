(ns nature-of-code.02-forces.exercise-02-04
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))

(def movers (atom ()))

(loop [times 50]
  (when (> times 0)
    (swap! movers #(conj % {:mass (+ 1 (rand-int 50))
                            :location [0 0]
                            :velocity [0 0]
                            :acceleration [0 0]}))
    (recur (dec times))))

(defn gravity [{:keys [mass]}]
  (vector 0 (* 0.1 mass)))

(defn friction [{:keys [velocity] [x y] :location}]
  (let [c (cond (> x 400) 0.01
                (> x 300) 0.2
                (> x 200) 5
                (> x 100) 0.2
                :else 0.01)]
    (-> velocity
        (v/mult -1)
        (v/normalize)
        (v/mult c))))

(defn update-mover [mover width height]
  (let [wind [0.1 0]]
    (-> mover
        (m/apply-force wind)
        (m/apply-force (gravity mover))
        (m/apply-force (friction mover))
        (m/compute-position)
        (m/check-edges width height))))

(defn draw []
  (q/clear)
  (q/fill 0 0 255)
  (q/rect 0 0 100 (q/height))
  (q/fill 0 255 0)
  (q/rect 100 0 100 (q/height))
  (q/fill 255 255 0)
  (q/rect 200 0 100 (q/height))
  (q/fill 0 255 255)
  (q/rect 300 0 100 (q/height))
  (q/fill 255 0 0)
  (q/rect 400 0 100 (q/height))
  (q/fill 255 0 255)
  (doseq [{:keys [mass] [x y] :location}
          (swap! movers #(map (fn [mover] (update-mover mover (q/height) (q/width))) %))]
    (q/ellipse x y mass mass)))

(q/defsketch friction
  :title "friction"
  :settings #(q/smooth 2)
  :draw draw
  :size [500 500])

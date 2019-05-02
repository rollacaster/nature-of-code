(ns nature-of-code.02-forces.exercise-02-06
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))

(def movers (atom ()))

(loop [times 10]
  (when (> times 0)
    (swap! movers #(conj % {:mass (+ 1 (rand-int 50))
                            :location [(* times 50) (* times 50)]
                            :velocity [0 0]
                            :acceleration [0 0]}))
    (recur (dec times))))

(defn liquid [x y w h c]
  {:location [x y]
   :size [w h]
   :c c})

(defn gravity [{:keys [mass]}]
  (vector 0 (- (* 0.1 mass))))

(defn drag [mover liquid]
  (let [{[x y] :location :keys [velocity mass]} mover
        {:keys [c location]} liquid
        speed (v/mag velocity)
        A mass]
    (if (> y (second location))
      (v/mult (v/mult (v/normalize (v/mult velocity -1))
                      (* c speed speed))
              (* 0.001 mass))
      [0 0])))

(defn update-mover [mover width height]
  (let [wind [0.1 0]]
    (-> mover
        (m/apply-force (gravity mover))
        (m/apply-force (drag mover (liquid 0 (/ (q/height) 2) (q/width) (/ (q/height) 2) 1)))
        (m/compute-position)
        (m/keep-inside width height))))

(defn draw []
  (q/clear)
  (q/background 255)
  (q/fill 50 100 255)
  (q/rect 0 (/ (q/height) 2) (q/width) (/ (q/height) 2))
  (q/fill 255 0 255)
  (doseq [{:keys [mass] [x y] :location}
          (swap! movers #(map (fn [mover] (update-mover mover (q/height) (q/width))) %))]
    (q/rect x (-  y mass) mass mass)))

(defn run []
  (q/defsketch surface-area
    :title "surface-area"
    :settings #(q/smooth 2)
    :draw draw
    :size [500 500]))

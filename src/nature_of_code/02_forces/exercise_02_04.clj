(ns nature-of-code.02-forces.exercise-02-04
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.middleware :as md]))

(defn setup []
  (q/frame-rate 1)
  {:movers (doall (map
                   (fn [mover] {:mass (+ 1 (rand-int 50))
                                :location [0 0]
                                :velocity [0 0]
                                :acceleration [0 0]})
                   (range 50)))})

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

(defn update-state [{:keys [movers]}]
  (println "movers" movers)
  {:movers (doall (map (fn [mover] (fn [mover] (update-mover mover (q/height) (q/width))))
                       movers))})

(defn draw [{:keys [movers]}]
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
  (doseq [{:keys [mass] [x y] :location} movers]
      (q/ellipse x y mass mass)))

(defn run []
  (q/defsketch friction
    :title "friction"
    :settings #(q/smooth 2)
    :draw draw
    :setup setup
    :update-state update-state
    :middleware [md/pause-on-error md/fun-mode]
    :size [700 500]))

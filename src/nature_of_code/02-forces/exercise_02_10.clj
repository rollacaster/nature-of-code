(ns nature-of-code.02-forces.exercise-02-10
  (:require [quil.core :as q]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]))


(def movers (atom (->> (range 0 10)
                       (map (fn [x] {:mass (+ 10 (rand-int 50))
                                     :location [(rand-int 500) (rand-int 500)]
                                     :velocity [0 0]
                                     :acceleration [0 0]})))))

(defn compute-movers []
  (let [mouse {:mass 10 :location [(q/mouse-x) (q/mouse-y)]}]
    (doseq [{:keys [mass] [x y] :location}
            (swap! movers
                   (fn [movers]
                     (map (fn [mover]
                            (m/compute-position
                             (m/apply-force
                              (reduce #(m/apply-force %1 (m/repulse %1 %2)) mover movers)
                              (m/attract mover mouse))))
                          movers)))]
      (q/ellipse x y mass mass))))

(defn draw []
  (q/clear) 
  (q/background 255)
  (compute-movers))

(q/defsketch repulse-mouse
  :title "repulse-mouse"
  :settings #(q/smooth 2)
  :draw draw
  :size [500 500])



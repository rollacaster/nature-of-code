(ns nature-of-code.03-oscillation.exercise-03-02
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [nature-of-code.mover :as m]))

(def cannonballs (atom (map #(m/create-mover (* % 5) [(* % 5) (rand-int 500)]) (range 0 10))))
(def attractor (m/create-mover 50 [200 200]))

(defn gravity [{:keys [mass]}]
  (vector 0 (* 0.1 mass)))

(defn draw []
  (q/clear)
  (q/background 255)
  (q/rect-mode :center)
  (q/ellipse-mode :center)
  (let [{:keys [mass angle] [x y] :location} attractor]
    (q/ellipse x y mass mass))
  (doseq [{:keys [mass angle] [x y] :location}
          (swap! cannonballs (fn [cannonballs]
                               (map
                                #(-> %
                                     #_(m/apply-force (gravity %))
                                     (m/apply-force (m/attract % attractor))
                                     (m/compute-position))
                                cannonballs)))]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/rect 0 0 mass mass)
    (q/pop-matrix)))

(q/defsketch cannonball
  :title "cannonball"
  :settings #(q/smooth 2)
  :draw draw
  :middleware [md/pause-on-error]
  :size [800 500])

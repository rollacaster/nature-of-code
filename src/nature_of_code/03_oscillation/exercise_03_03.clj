(ns nature-of-code.03-oscillation.exercise-03-03
  (:require [nature-of-code.mover :as m]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
      (q/rect-mode :center)
  (m/create-mover 100 [100 100]))

(defn cart-to-pol [[x y]]
  [(Math/sqrt (+ (* x x) (* y y))) (q/atan2 y x)])

(defn pol-to-cart [[r phi]]
  [(* r (q/cos phi)) (* r (q/sin phi))])

(defn steer [{[x y] :velocity :as mover} key]
  (let [[r phi] (cart-to-pol [x y])]
       (cond
         (= key :left) (pol-to-cart [r (+ phi 10)])
         (= key :right) (pol-to-cart [r (- phi 10)])
         (= key :up) (pol-to-cart [(+ r 5) phi])
         (= key :down) (pol-to-cart [(- r 5) phi])
         :else [x y])))

(defn draw [car]
  (q/background 255)
  (q/fill 123)
  (q/stroke 255)
  (q/rect-mode :center)
  (let [{:keys [mass]
         [x y] :location
         [vx vy] :velocity} car
        angle (q/atan2 vy vx)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/rect 0 0 mass (* 0.5 mass))
    (q/pop-matrix))
  (m/draw-stats car))

(defn update-state [car]
  (-> car
      m/move-through
      m/compute-position)
  #_(m/create-mover 100 [100 100]))

(defn key-pressed [car {:keys [key]}]
  (m/apply-force car (steer car key)))

(defn run []
  (q/defsketch car-sketch
    :title "car"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :features [:no-bind-output]
    :key-pressed key-pressed
    :setup setup
    :draw draw
    :update update-state
    :size [750 500]))



(ns nature-of-code.03-oscillation.exercise-03-05
  (:require [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn setup []
  (m/create-mover 50 [250 250]))

(defn draw [spaceship]
  (q/background 255)
  (q/fill 126)
  (q/stroke-weight 2)
  (let [{:keys [mass angle] [x y] :location} spaceship]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/triangle (/ mass 2) 0 (- (/ mass 2)) (- (/ mass 2)) (- (/ mass 2)) (/ mass 2))
    (q/rect (- (- (/ mass 2)) 5) 5 5 5)
    (q/rect (- (- (/ mass 2)) 5) -5 5 5)
    (q/pop-matrix)))

(defn update-state [{:keys [velocity] :as spaceship}]
  (-> spaceship
      (m/apply-force (v/mult velocity -0.2))
      (m/move-through)
      (m/compute-position)))

(defn key-pressed [{:keys [angle] :as spaceship} {:keys [key]}]
  (cond (= (q/key-as-keyword) :left) (update spaceship :angle + 0.2)
        (= (q/key-as-keyword) :right) (update spaceship :angle - 0.2)
        (= (q/key-as-keyword) :up) (assoc spaceship :acceleration [(q/cos angle) (q/sin angle)])
        :else spaceship))

(defn run []
  (q/defsketch asteriods
    :title "asteriods"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :update update-state
    :key-pressed key-pressed
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

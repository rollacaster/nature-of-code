(ns nature-of-code.04-particle-systems.exercise-04-04
  (:require [nature-of-code.04-particle-systems.exercise-04-03 :as p]
            [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(def spaceship (atom (m/create-mover 50 [250 250])))
(def angle (atom 0))
(def particles (atom ()))

(defn setup []
  )

(defn draw []
  (q/background 255)
  (q/fill 126)
  (q/stroke-weight 2)
  (let [{:keys [mass angle]
         [x y] :location} (swap! spaceship #(-> %
                                                (m/apply-force (v/mult (:velocity %) -0.2))
                                                (m/move-through (q/width) (q/height))
                                                (m/compute-position)))]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/triangle (/ mass 2) 0 (- (/ mass 2)) (- (/ mass 2)) (- (/ mass 2)) (/ mass 2))
    (q/rect (- (- (/ mass 2)) 2) 5 5 5)
    (q/rect (- (- (/ mass 2)) 2) -5 5 5)
    (when (and (q/key-pressed?) (= (q/key-as-keyword) :up))
      (swap! particles
             #(->> (conj % (p/add-origin [-40 0] (p/create-particle [0 0]))))))
    (p/run-particle-system particles -40 0 [-0.05 0])
    (q/pop-matrix)))

(defn key-pressed []
  (swap! spaceship #(cond (= (q/key-as-keyword) :left) (update % :angle (fn [angle] (+ angle 0.2)))
                          (= (q/key-as-keyword) :right) (update % :angle (fn [angle] (- angle 0.2)))
                          (= (q/key-as-keyword) :up)
                          (assoc % :acceleration [(q/cos (:angle %))
                                                  (q/sin (:angle %))])
                          :else %)))

(defn run []
  (q/defsketch asteriods
    :title "asteriods"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :key-pressed key-pressed
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))


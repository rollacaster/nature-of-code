(ns nature-of-code.01-vectors.exercise-01-02
  (:require [quil.core :as q]))

(defn add [v1 v2]
  (hash-map :x (+ (:x v1) (:x v2))
            :y (+ (:y v1) (:y v2))))

(defn setup []
  (def location (atom (hash-map :x (/ (q/width) 2) :y (/ (q/height) 2))))
  (def tx (atom 0))
  (q/background 255))

(defn draw []
  (q/stroke 0)
  (let [tx (swap! tx #(+ 0.01 %))
        choice (rand-int 4)
        step (q/map-range (q/noise tx) 0 1 0 10)
        velocity (cond (= choice 0) {:x step :y 0}
                       (= choice 1) {:x (- step) :y 0}
                       (= choice 2) {:x 0 :y step}
                       :else {:x 0 :y (- step)})]
    (let [{:keys [x y]} (swap! location #(add % velocity))]
      (q/point x y))))

(q/defsketch ramdom-walk-vector
  :title "ramdom-walk-vector"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])

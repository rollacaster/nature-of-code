(ns nature-of-code.03-oscillation.exercise-03-10
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(def waves (atom '({:amplitude [300 200]
                    :angleVel 0.1
                    :startAngle 0
                    :location [50 50]}
                   {:amplitude [500 50]
                    :angleVel 0.3
                    :startAngle 0
                    :location [50 300]})))

(defn draw-wave [wave]
  (let [{:keys [angleVel startAngle]
         [l1 l2] :location
         [a1 a2] :amplitude} wave]
    (let [angle (atom startAngle)]
      (doseq [i (range (/ a1 24))
              :let [x (* i 24)]]
        (let [y (q/map-range (q/sin @angle) -1 1 0 a2)]
          (q/stroke 0)
          (q/fill 0 50)
          (q/ellipse (+ l1 x) (+ l2 y) 48 48)
          (swap! angle #(+ % angleVel)))))
    (update wave :startAngle #(+ % 0.02))))

(defn setup []
  )

(defn draw []
  (q/background 255)
  (doseq [wave (swap! waves (fn [waves] (map draw-wave waves)))]))

(q/defsketch wave
  :title "wave"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

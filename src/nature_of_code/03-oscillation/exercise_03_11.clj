(ns nature-of-code.03-oscillation.exercise-03-11
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(def waves (atom '({:amplitude 200
                    :angleVel 0.1
                    :startAngle 0}
                   {:amplitude 50
                    :angleVel 0.3
                    :startAngle 0}
                   {:amplitude 150
                    :angleVel 0.6
                    :startAngle 0})))

(defn combine [waves]
  (reduce
   (fn [c-wave wave]
     (let [{:keys [angleVel startAngle amplitude]} wave]
       (let [angle (atom startAngle)]
         (map-indexed
          (fn [i [x y]]
            (swap! angle #(+ % angleVel))
            [(* i 24) (+ y (q/map-range (q/sin @angle) -1 1 0 amplitude))])
          c-wave))))
   (map (fn [i] [0 0]) (range (/ (q/width) 24)))
   waves))

(defn combine-wave [waves]
  (let [c-wave (combine waves)]
       (doseq [[x y] c-wave]
         (q/stroke 0)
         (q/fill 0 50)
         (q/ellipse x y 48 48)))
  (map (fn [wave] (update wave :startAngle #(+ % 0.02))) waves))
(combine-wave @waves)
(defn setup []
  )

(defn draw []
  (q/background 255)
  (q/translate 0 125)
  (doseq [wave (swap! waves combine-wave)]))

(q/defsketch combine-wave
  :title "combine-wave"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

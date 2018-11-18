(ns nature-of-code.03-oscillation.exercise-03-03
  (:require [nature-of-code.mover :as m]
            [quil.core :as q]
            [quil.middleware :as md]))

(def car (atom (m/create-mover 100 [100 100])))

(defn setup [])

(defn steer [mover]
  (update mover :acceleration #(cond
                             (= (q/key-as-keyword) :left) (let [[x y] %] [(dec x) y])
                             (= (q/key-as-keyword) :right) (let [[x y] %] [(inc x) y])
                             (= (q/key-as-keyword) :up) (let [[x y] %] [x (dec y)])
                             (= (q/key-as-keyword) :down) (let [[x y] %] [x (inc y)])
                             :else %)))

(defn draw []
  (q/background 255)
  (q/fill 123)
  (q/stroke 255)
  #_(when (q/key-pressed?)
   (println (q/key-as-keyword) (q/frame-count)))
  (let [attractor (m/create-mover 50 [(q/mouse-x) (q/mouse-y)])
        {:keys [mass velocity]
         [x y] :location} (swap! car
                                 #(-> %
                                      (m/compute-position)))
        angle (q/atan2 (second velocity) (first velocity))]
    (q/push-matrix)
    (q/rect-mode :center)
    (q/translate x y)
    (q/rotate angle)
    (q/rect 0 0 mass (* 0.5 mass))
    (q/pop-matrix)))

(q/defsketch car
  :title "car"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :features [:no-bind-output]
  :key-pressed #(swap! car (fn [car] (-> car
                                         (steer)
                                         (m/keep-inside (q/width) (q/height))
                                         (m/compute-position))))
  :setup setup
  :draw draw
  :size [750 500])



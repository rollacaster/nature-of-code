(ns nature-of-code.10-neural-networks.example-10-01
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn activate [sum]
  (if (> sum 0) 1 -1))

(defn feedforward [inputs weights]
  (let [sum (reduce
           +
           (map-indexed
            (fn [idx input]
              (* input (nth weights idx)))
            inputs))]
    (activate sum)))

(defn setup-perceptron [n]
  (map
   (fn [_] (- (rand 2) 1))
   (range n)))

(defn train [inputs weights desired c]
  (let [guess (feedforward inputs weights)
        error (- desired guess)]
    (map-indexed
     (fn [idx weight]
       (+ weight (* c error (nth inputs idx))))
     weights)))

(defn trainer [x y answer]
  {:inputs [x y 1]
   :answer answer})

(defn f [x] (+ (* 2 x) 1))

(defn setup []
  {:counter 0
   :ptron (setup-perceptron 3)
   :training (map
              (fn [_]
                (let [x (q/random (- (/ (q/width) 2)) (/ (q/width) 2))
                      y (q/random (- (/ (q/height) 2)) (/ (q/height) 2))
                      answer (if (< y (f x)) -1 1)]
                  (trainer x y answer)))
              (range 2000))})

(defn update-state [{:keys [training counter ptron] :as state}]
  (-> state
      (assoc :ptron
             (train (nth (map :inputs training) counter)
                    ptron
                    (nth (map :answer training) counter)
                    0.01))
      (update :counter #(mod (inc %) (count training)))))

(defn draw [{:keys [counter training ptron]}]
  (q/background 255)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [i (range counter)]
    (let [training-inputs (:inputs (nth training i))
          guess (feedforward training-inputs ptron)
          [x y] training-inputs]
      (if (> guess 0)
        (q/no-fill)
        (q/fill 0))
      (q/ellipse x y 8 8))))

(defn run []
  (q/defsketch perceptron
    :title "perceptron"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :display 1
    :features [:no-bind-output]
    :size [700 500]))


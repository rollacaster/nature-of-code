(ns nature-of-code.08-fractals.exercise-08-12
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [clojure.string :as s]))

;; NOT FINISHED needs to converted according to Exercises8.12

(def rules {"F" "FF+[+F-F-F]-[-F+F+F]"})

(defn l-system [current-sentence rules]
  (apply str
         (reduce
          (fn [next-sentence sentence]
            (concat next-sentence (or (get rules (str sentence)) (str sentence))))
          ""
          current-sentence)))

(defn turtle [sentence len angle]
  (doseq [letter sentence]
    (case letter
      \F (do (q/line 0 0 len 0)
             (q/translate len 0))
      \G (q/translate len 0)
      \+ (q/rotate angle)
      \- (q/rotate (- angle))
      \[ (q/push-matrix)
      \] (q/pop-matrix))))

(defn setup []
  {:sentence "F" :length (/ (q/width) 4)})

(defn draw [{:keys [sentence length]}]
  (q/background 255)
  (q/translate (/ (q/width) 2) (q/height))
  (q/rotate (- q/HALF-PI))
  (turtle sentence length (q/radians 25)))

(defn mouse-pressed [state ev]
  (-> state
      (update :sentence l-system rules)
      (update :length / 2)))

(q/defsketch l-system-sketch
  :title "l-system"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed
  :display 1
  :features [:no-bind-output]
  :size [700 500])

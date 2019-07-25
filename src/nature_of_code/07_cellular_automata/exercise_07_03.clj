(ns nature-of-code.07-cellular-automata.exercise-07-03
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup-cells []
  (let [w 5
        cell-count (/ 700 w)]
    (map-indexed (fn [idx x] (q/round (q/random 1)))
                 (range cell-count))))

(defn setup []
  {:cells (setup-cells)
   :ruleset [0 1 0 1 1 0 1 0]
   :generation 0})

(defn rules [ruleset a b c]
  (get ruleset (q/unbinary (str a b c))))

(defn reset-state [state]
  (-> state
      (assoc :generation 0)
      (assoc :cells (setup-cells))
      (assoc :ruleset (into [] (map (fn [x] (q/round (q/random 1))) (range 8))))))

(defn update-automata [{:keys [cells ruleset] :as state}]
  (-> state
      (assoc :cells (map-indexed (fn [idx x]
                                   (cond (= idx 0) 0
                                         (= idx (- (count cells) 1)) 0
                                         :else (rules ruleset
                                                      (nth cells (dec idx))
                                                      (nth cells idx)
                                                      (nth cells (inc idx)))))
                                 cells))
      (update :generation inc)))

(defn update-state [{:keys [generation] :as state}]
  (if (> generation (/ (q/height) 5))
    (reset-state state)
    (update-automata state)))

(defn draw [{:keys [cells generation]}]
  (doall
   (map-indexed
    (fn [idx cell]
      (if (= cell 1) (q/fill 0) (q/fill (q/random 255) (q/random 255) (q/random 255)))
      (q/rect (* idx 5) 0 5 (q/height)))
    cells)))

(q/defsketch ca-random
  :title "ca-random"
  :display 1
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :update update-state
  :features [:no-bind-output]
  :size [700 500])

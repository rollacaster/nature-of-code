(ns nature-of-code.07-cellular-automata.exercise-07-04
  (:require [quil.core :as q]
            [clojure.pprint :as p]
            [clojure.string :as s]
            [quil.middleware :as md]))

(defn setup-cells []
  (let [w 5
        cell-count (/ 700 w)]
    (map-indexed (fn [idx x] (q/round (q/random 1)))
                 (range cell-count))))

(defn setup []
  {:cells [(setup-cells)]
   :ruleset [0 1 0 1 1 0 1 0]})

(defn rules [ruleset a b c]
  (get ruleset (q/unbinary (str a b c))))

(defn update-state [{:keys [cells ruleset] :as state}]
  (let [new-cells (map-indexed (fn [idx x]
                                 (cond (= idx 0) 0
                                       (= idx (- (count (last cells)) 1)) 0
                                       :else (rules ruleset
                                                    (nth (last cells) (dec idx))
                                                    (nth (last cells) idx)
                                                    (nth (last cells) (inc idx)))))
                               (last cells))
        new-state (assoc state :cells
           (conj (if (<= (count cells) (/ (q/height) 5))
                   cells
                   (into [] (rest cells)))
                 new-cells))]
    new-state))

(defn draw-cell [y x cell]
  (if (= cell 1) (q/fill 0) (q/fill 255))
  (q/rect (* x 5) (* 5 y) 5 5))

(defn draw [{:keys [cells]}]
  (doall (map-indexed
          (fn [idx line]
            (doall (map-indexed (partial draw-cell idx) line)))  cells)))

(defn run []
  (q/defsketch ca-scroll
    :title "ca-scroll"
    :display 1
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :size [700 500]))



(ns nature-of-code.09-the-evolution-of-code.exercise-09-06
  (:require [quil.core :as q]
            [quil.middleware :as md]
            [clojure.string :as s]))

(defn rand-gene []
  (char (+ (rand-int 96) 32)))

(defn setup-dna []
  {:genes (map (fn [i] (rand-gene)) (range 18))})

(defn fitness [target genes]
  (/
   (loop [idx 0
          score 0]
     (if (and (< idx (count genes)) (< idx (count target)))
       (if (= (nth genes idx) (nth target idx))
         (recur (inc idx) (inc score))
         (recur (inc idx) score))
       score))
   (count target)))

(defn mating [population]
  (for [{:keys [fitness] :as dna} population
          n (range (int (* 100 fitness)))]
      dna))

(defn cross-over [dna partner]
  (let [midpoint (rand-int (count (:genes dna)))]
    {:genes
     (concat
      (take midpoint (:genes dna))
      (drop midpoint (:genes partner)))}))

(defn mutate [dna mutation-rate]
  (update dna :genes #(map (fn [gene]
                             (if (< (rand) mutation-rate)
                               (rand-gene)
                               gene)) %)))

(defn initialize-population [total-population]
  (for [_ (range total-population)] (setup-dna)))

(defn selection [target population]
  (map (fn [{:keys [genes] :as dna}] (assoc dna :fitness (fitness target genes))) population))

(defn reproduction [mutation-rate population mating-pool]
  (map
   (fn [_]
     (let [i1 (rand-int (count mating-pool))
           i2 (rand-int (count mating-pool))
           child (cross-over (nth mating-pool i1) (nth mating-pool i2))]
       (mutate child mutation-rate)))
   population))

(defn setup []
  (q/frame-rate 1)
  (let [total-population 150]
    {:target (seq (char-array "to be or not to be"))
     :mutation-rate 0.01
     :generation 0
     :population (initialize-population total-population)}))

(defn update-state [{:keys [population target mutation-rate] :as state}]
  (let [population-selection (selection target population)
        mating-pool (mating population-selection)
        new-population (reproduction mutation-rate population-selection mating-pool)]
    (-> state
        (assoc :population new-population)
        (update :generation inc))))

(defn draw [{:keys [population target generation mutation-rate]}]
  (let [population-selection (selection target population)
        best-phrase (apply str (:genes (last (sort-by :fitness population-selection))))
        average-fitness (format "%.2f"
                                (/ (reduce (fn [fitness dna] (+ fitness (:fitness dna))) 0.0 population-selection)
                                   (count population-selection)))
        total-population (count population-selection)
        mutation-rate (format "%.2f" mutation-rate)
        generation (str generation)
        all-phrases (map #(apply str (:genes %)) population)]
    (q/background 127)
    (q/fill 255)
    (q/text-size 16)
    (q/text "Best Phrase:" 100 80)
    (q/text-size 25)
    (q/text best-phrase 100 110)
    (q/text-size 16)
    (q/text (str "total generations: " generation) 100 150)
    (q/text (str "average fitness: " average-fitness) 100 170)
    (q/text (str "total population: " (count population-selection)) 100 190)
    (q/text (str "mutation rate: " mutation-rate) 100 210)
    (q/text "All phrases:" 400 80)
    (doall
     (map-indexed
      (fn [i phrase]
        (q/text phrase 400 (+ (* i 20) 110)))
      (take 15 all-phrases)))
    (q/text "..." 400 (+ (* 15 20) 100))))

(defn run []
  (q/defsketch mutation-report
    :title "mutation-report"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup setup
    :draw draw
    :update update-state
    :display 1
    :features [:no-bind-output]
    :size [700 500]))








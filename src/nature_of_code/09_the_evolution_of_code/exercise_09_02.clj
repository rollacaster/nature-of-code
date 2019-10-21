(ns nature-of-code.09-the-evolution-of-code.exercise-09-02
  (:require [quil.core :as q]
            [quil.middleware :as md]))

(defn setup-dna []
  {:phrase (map (fn [i] (char (+ (rand-int 26) 97))) (range 18))})


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

(defn mating-monte-carlo [population]
  (filter
   #(not (nil? %))
   (for [{:keys [fitness] :as dna} population]
     (let [r (rand)]
       (when (> 0.5 r)
         dna)))))

(defn setup []
  (let [target '(\u \l \s \x \o \v \i \f \t \o \i \l \u \y \e \s \c \l)
        population (map
                    (fn [{:keys [phrase] :as dna}]
                      (assoc dna :fitness (fitness target phrase)))
                    (for [_ (range 100)] (setup-dna)))]
    (mating-monte-carlo population)))

(defn update-state [state]
  )

(defn draw [state]
  )

(q/defsketch genetic-alg
  :title "genetic-alg"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :update update-state
  :display 1
  :features [:no-bind-output]
  :size [700 500])

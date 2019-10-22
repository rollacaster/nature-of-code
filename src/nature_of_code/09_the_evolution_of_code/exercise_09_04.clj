(ns nature-of-code.09-the-evolution-of-code.exercise-09-03)

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

(defn reproduction [mating-pool]
  (let [i1 (rand-int (count mating-pool))
        i2 (rand-int (count mating-pool))]
    (if (= i1 i2)
      (reproduction mating-pool)
      [(nth mating-pool i1)
       (nth mating-pool i2)])))

(defn cross-over [dna partner]
  (let [midpoint (rand-int (count (:phrase dna)))]
    {:phrase
     (concat
      (take midpoint (:phrase dna))
      (drop midpoint (:phrase partner)))}))

(defn cross-over-coin-flip [dna partner]
  {:phrase
   (map-indexed
    (fn [idx v]
      (if (> (rand) 0.5)
        v
        (nth (:phrase partner) idx)))
    (:phrase dna))})

(defn run []
  (let [target '(\u \l \s \x \o \v \i \f \t \o \i \l \u \y \e \s \c \l)
        population (map
                    (fn [{:keys [phrase] :as dna}]
                      (assoc dna :fitness (fitness target phrase)))
                    (for [_ (range 100)] (setup-dna)))
        mating-pool (mating population)]
    (reproduction mating-pool)))







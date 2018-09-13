(ns nature-of-code.01-vectors.exercise-01-04)

(defn normalize [v]
  (let [m (mag v)]
    (when (not (= m 0)) (div v m))))

(defn mag [[x y]] (Math/sqrt (+ (* x x) (* y y))))

(defn limit [[x y] top]
  (if (> (mag [x y]) top)
    (mult (normalize [x y]) top)
    [x y]))

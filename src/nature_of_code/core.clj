(ns nature-of-code.core)

(defn add [v1 v2]
  (vector (+ (first v1) (first v2))
          (+ (second v1) (second v2))))

(defn sub [v1 v2]
  (vector (- (first v1) (first v2))
          (- (second v1) (second v2))))

(defn mult [v1 n] (vector (* (first v1) n) (* (second v1) n)))

(defn div [v1 n] (vector (/ (first v1) n) (/ (second v1) n)))

(defn mag [[x y]] (Math/sqrt (+ (* x x) (* y y))))

(defn limit [[x y] top]
  (if (> (mag [x y]) top)
    (mult (normalize [x y]) top)
    [x y]))

(defn normalize [v]
  (let [m (mag v)]
    (when (not (= m 0)) (div v m))))


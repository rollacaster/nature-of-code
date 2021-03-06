(ns nature-of-code.vector)

(defn add
  ([v] v)
  ([[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])
  ([v1 v2 & vs]
   (apply add (add v1 v2) vs)))

(defn sub [v1 v2]
  (vector (- (first v1) (first v2))
          (- (second v1) (second v2))))

(defn mult [v1 n] (vector (* (first v1) n) (* (second v1) n)))

(defn div [[x y] n] (if (or (= n 0) (= n 0.0))
                      (vector x y)
                      (vector (/ x n) (/ y n))))

(defn mag [[x y]] (Math/sqrt (+ (* x x) (* y y))))

(defn normalize [v]
  (let [m (mag v)]
    (if (not (= m 0.0)) (div v m) v)))

(defn limit [[x y] top]
  (if (> (mag [x y]) top)
    (mult (normalize [x y]) top)
    [x y]))

(defn dot-product [[x1 y1] [x2 y2]]
  (+ (* x1 x2) (* y1 y2)))

(defn angle-between [v1 v2]
  (Math/acos (/ (dot-product v1 v2) (* (mag v1) (mag v2)))))

(defn round [[x1 x2]]
  [(Math/round x1) (Math/round x2)])

(defn rotate [[x y] angle]
  [(- (* (Math/cos angle) x) (* (Math/sin angle) y))
   (+ (* (Math/sin angle) x) (* (Math/cos angle) y))])


(ns nature-of-code.05-physics-libraries.exercise-05-06
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 100]))
(def pairs (atom ()))

(defn create-chain [world surface]
  (let [body (b/body! world {:type :static} {:shape (b/edge-chain surface) :restitution 0.5})]
    {:body body}))

(defn create-pair [world x y]
  (let [p1 (b/body! world {:position [x y]} {:shape (b/box 8 8) :restitution 0.5})
        pos2 [(+ x (q/random -8 8)) (+ y (q/random -8 8))]
        p2 (b/body! world {:position pos2} {:shape (b/box 8 8) :restitution 0.5})
        djd (b/joint!* {:type :distance
                        :body-a p1 :anchor-a [x y]
                        :body-b p2 :anchor-b pos2 :length 32})]
    {:p1 p1 :p2 p2 :line [[x y] pos2]}))

(defn create-bridge [world width height]
  (let [circle-count (/ width 20)]
    (reduce
     (fn [bridge circle]
       (let [body
             (b/body! world
                      {:position [(* circle 10) 0]
                       :type (if (or (= circle 0) (= circle 34))
                               :static :dynamic)}
                      {:shape (b/circle 8 [(* circle 10) (/ height 2)])})]
         (if-let [pre (first bridge)]
           (b/joint!* {:type :distance :body-a pre :body-b body :length 10
                       :anchor-a [(* (- circle 1.0) 10) (/ height 2)]
                       :anchor-b [(* circle 10) (/ height 2)]}))
         (conj bridge body)))
     '()
     (range circle-count))))

(def bridge (create-bridge world 700 500))

(defn display-pair [{:keys [p1 p2 line]}]
  (let [vert1 (b/world-coords (b/fixture-of p1))
        vert2 (b/world-coords (b/fixture-of p2))]
    (q/line (first vert1) (first vert2))
    (q/begin-shape)
    (doseq [v vert1]
      (apply q/vertex v))
    (q/end-shape :close)
    (q/begin-shape)
    (doseq [v vert2]
      (apply q/vertex v))
    (q/end-shape :close)))

(defn display-bridge [bridge]
  (doall (map (fn [body]
                (let [vertices (b/world-coords (b/fixture-of body))]
                  (q/stroke-weight 1)
                  (q/stroke 0)
                  (q/no-fill)
                  (q/begin-shape)
                  (doseq [v vertices]
                    (apply q/vertex v))
                  (q/end-shape)))
              bridge)))

(defn setup []
  )

(defn draw []
  (q/clear)
  (q/background 127)
  (b/step! world (/ 1.0 60.0))
  (display-bridge bridge)
  (when (q/mouse-pressed?)
    (swap! pairs conj (create-pair world (q/mouse-x) (q/mouse-y))))
  (doall (map display-pair @pairs)))

(q/defsketch physics-distance-joint
  :title "physics-distance-joint"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

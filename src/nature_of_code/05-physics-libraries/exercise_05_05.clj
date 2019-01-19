(ns nature-of-code.05-physics-libraries.exercise-05-05
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 100]))
(def boxes (atom ()))

(defn create-alien [world x y]
  (let [body (b/body! world {:position [x y]}
                      {:shape (b/box 8 8) :restitution 0.5}
                      {:shape (b/circle 8 [0 16])})]
    {:body body :w 16 :h 16}))

(defn create-chain [world surface]
  (let [body (b/body! world
                      {:type :static}
                      {:shape (b/edge-chain surface) :restitution 0.5})]
    {:body body :surface surface}))

(def curved-boundary (create-chain world
                                   (map #(vec [%
                                               (let [y (q/sin (q/map-range % 0 700 0 12.56))]
                                                 (+ 350 (* 100 y)))])
                                        (range 700))))

(defn display-box [{:keys [body w h]}]
  (let [[circle rect] (b/fixtureseq body)
        [[x y]] (b/world-coords rect)
        angle (b/angle body)]
    (q/fill 175)
    (q/stroke 0)
    (q/begin-shape)
    (doseq [[x y] (b/world-coords rect)]
      (q/vertex x y))
    (q/end-shape :close)
    (q/begin-shape)
    (doseq [[x y] (b/world-coords circle)]
      (q/vertex x y))
    (q/end-shape :close)))

(defn display-surface [{:keys [body surface]}]
  (let [vertices (b/world-coords (b/fixture-of body))]
    (q/stroke-weight 1)
    (q/stroke 0)
    (q/no-fill)
    (q/begin-shape)
    (doseq [v surface]
      (apply q/vertex v))
    (q/end-shape)))

(defn setup [])
(defn draw []
  (q/clear)
  (q/background 127)
  (b/step! world (/ 1.0 60.0))
  (display-surface curved-boundary)
  (when (q/mouse-pressed?)
    (swap! boxes conj (create-alien world (q/mouse-x) (q/mouse-y))))
  (doall (map display-box @boxes)))

(q/defsketch physics
  :title "physics"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700, 500])

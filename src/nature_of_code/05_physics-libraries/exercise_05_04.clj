(ns nature-of-code.05-physics-libraries.exercise-05-03
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 100]))
(def boxes (atom ()))

(defn create-box [world x y vertices]
  (let [body (b/body! world {:position [x y]} {:shape (b/polygon vertices) :restitution 0.5})]
    {:body body}))

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

(defn display-box [{:keys [body]}]
  (let [vertices (b/world-coords (b/fixture-of body))]
    (q/fill 175)
    (q/stroke 0)
    (q/begin-shape)
    (doseq [[x y] vertices]
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
    (swap! boxes conj (create-box world (q/mouse-x) (q/mouse-y)
                                  [[-15 25] [15 0] [20 -15] [-10 -10]])))
  (doall (map display-box @boxes)))

(defn run []
  (q/defsketch physics
    :title "physics"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :size [700, 500]))

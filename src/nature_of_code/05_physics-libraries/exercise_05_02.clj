(ns nature-of-code.05-physics-libraries.exercise-05-02
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn init []
  (def world (b/new-world [0 100]))
  (def boundary (create-boundary world 350 250 350 10))
  (def boxes (atom ())))

(defn coord-pixels-to-world [x y]
  (let [scale-factor 10.0
        worldX (q/map-range x (/ 700 2.0) (+ (/ 700 2.0) scale-factor), 0.0 1.1)
        worldY (q/map-range y (/ 500 2.0) (+ (/ 500 2.0) scale-factor) 0.0 1.1)]
    [worldX worldY]))

(defn scalar-pixels-to-world [val]
  (/ val 10.0))

(defn create-box [world x y]
  (let [body (b/body! world {:position [x y]} {:shape (b/box 8 8) :restitution 0.5})]
    {:body body :w 16 :h 16}))

(defn create-boundary [world x y w h]
  (let [body (b/body! world
                      {:type :static :position [x y]}
                      {:shape (b/box (/ w 2)
                                     (/ h 2))})]
    {:body body :x x :y y :w w :h h}))

(defn display-box [{:keys [body w h]}]
  (let [[[x y]] (b/world-coords (b/fixture-of body))
        angle (b/angle body)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate angle)
    (q/fill 175)
    (q/stroke 0)
    (q/rect-mode :corner)
    (q/rect 0 0 w h)
    (q/pop-matrix)))

(defn display-boundary [{:keys [body x y w h]}]
  (let [[[x y]] (b/world-coords (b/fixture-of body))]
    (q/fill 175)
    (q/stroke 0)
    (q/rect-mode :corner)
    (q/rect x y w h)))

(defn setup [])

(defn draw []
  (q/clear)
  (q/background 127)
  (b/step! world (/ 1.0 60.0))
  (.clearForces world)
  (display-boundary boundary)
  
  (doall (map display-box @boxes)))

(defn run []
  (q/defsketch physics
    :mouse-pressed #(swap! boxes conj (create-box world (q/mouse-x) (q/mouse-y)))
    :title "physics"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error]
    :setup setup
    :draw draw
    :features [:no-bind-output]
    :size [700 500]))

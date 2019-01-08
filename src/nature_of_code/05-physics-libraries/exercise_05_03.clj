(ns nature-of-code.05-physics-libraries.exercise-05-03
  (:require [org.nfrac.cljbox2d.core :as b]
            [quil.core :as q]
            [quil.middleware :as md]))

(def world (b/new-world [0 10]))
(def boxes (atom ()))

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

(defn create-chain [world surface]
  (let [body (b/body! world
                      {:type :static}
                      {:shape (b/edge-chain surface) :restitution 0.5})]
    {:body body :surface surface}))

(def curved-boundary (atom nil))

(defn display-box [{:keys [body w h]}]
  (let [[[x y]] (b/world-coords (b/fixture-of body))
        angle (b/angle body)]
    (q/push-matrix)
    (q/translate x y)
    (q/rotate (- angle))
    (q/fill 175)
    (q/stroke 0)
    (q/rect-mode :center)
    (q/rect 0 0 w h)
    (q/pop-matrix)))

(defn display-surface [{:keys [surface]}]
  (q/stroke-weight 1)
  (q/stroke 0)
  (q/no-fill)
  (q/begin-shape)
  (doseq [v surface]
    (apply q/vertex v))
  (q/end-shape))

(defn setup []
  (reset! curved-boundary (create-chain world
                                   (map #(vec [%
                                               (let [y (q/sin (q/map-range % 0 700 0 12.56))]
                                                 (+ (* 100 (q/noise  (q/map-range % 0 700 0 10)))
                                                    350
                                                    (* 100 y)))])
                                        (range 700)))))
(defn draw []
  (q/clear)
  (q/background 127)
  (b/step! world 1)
  (display-surface @curved-boundary)
  (when (q/mouse-pressed?)
    (swap! boxes conj (create-box world (q/mouse-x) (q/mouse-y))))
  (doall (map display-box @boxes)))

(q/defsketch physics
  :title "physics"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error]
  :setup setup
  :draw draw
  :features [:no-bind-output]
  :size [700 500])

(ns nature-of-code.03-oscillation.exercise-03-16
  (:require [nature-of-code.mover :as m]
            [nature-of-code.vector :as v]
            [quil.core :as q]
            [quil.middleware :as md]))

(defn create-spring [x y l]
  {:location [x y] :len l})

(defn connect [bob spring]
  (let [k 0.1
        force (v/sub (:location bob) (:location spring))
        d (v/mag force)
        stretch (- d (:len spring))]
    (m/apply-force bob (v/mult (v/normalize force) (* -1 k stretch)))))

(defn setup []
  [{:spring (create-spring 350 0 100)
    :bob (m/create-mover 50 [300 50])}
   {:spring (create-spring 250 0 100)
    :bob (m/create-mover 50 [200 50])}])

(defn update-state [state]
  (reduce (fn [springs {:keys [spring bob]}]
            (conj springs
                  {:bob
                   (-> bob
                       (m/apply-force [0 1])
                       (connect spring)
                       (m/compute-position))
                   :spring (if-let [last-bob-location (-> springs
                                                          last
                                                          :bob
                                                          :location)]
                             (assoc spring :location last-bob-location)
                             spring)}))
          []
          state))

(defn draw [state]
  (q/background 255)
  (q/rect-mode :center)
  (doseq [{:keys [spring bob]} state]
    (let [{:keys [len] [a1 a2] :location} spring
          {:keys [mass] [x y] :location} bob]
      (q/line x y a1 a2)
      (q/rect a1 a2 10 10)
      (q/ellipse x y mass mass))))

(q/defsketch multiple-springs
  :title "multiple-springs"
  :settings #(q/smooth 2)
  :middleware [md/pause-on-error md/fun-mode]
  :setup setup
  :draw draw
  :display 1
  :update update-state
  :features [:no-bind-output]
  :size [700 500])

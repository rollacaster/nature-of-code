(ns nature-of-code.09-the-evolution-of-code.exercise-09-01)

(defn run []
  (time
   (loop [i 0]
     (if (= i 100000)
       (println "end")
       (if (= "cat" (apply str (map (fn [i] (char (+ (rand-int 26) 97))) (range 3))))
         (println i "cat!")
         (recur (inc i)))))))




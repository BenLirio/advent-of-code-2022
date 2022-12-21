(ns p2t2
  (:require [clojure.string :as str])
  (:gen-class))

;; ================ Utils ================
(defn max-elem [xs] (reduce max xs))
(defn min-elem [xs] (reduce min xs))

;; ================ Parser ================
(defn parse-line [s] (map #(Integer/parseInt %) (str/split s #"\,")))
(defn parse [s] (set (map parse-line (str/split s #"\n"))))

;; ================ Debug ================
(defn points-bounds [points]
  (apply map list (cons
    (map min-elem (apply map list points))
    [(map max-elem (apply map list points))])))

(defn info-input-bounds [points] (pr-str (points-bounds points)))

(defn info-area [points]
  (let [[[x1 x2] [y1 y2] [z1 z2]] (points-bounds points)]
    (* (- (+ x2 1) x1) (* (- (+ y2 1) y1) (- (+ z2 1) z1)))))

(defn info-input [points]
  (format "%d points\nBounds: %s\nArea: %s\n"
    (count points)
    (info-input-bounds points)
    (info-area points)))

;; ================ Solver ================
(defn neighbors [[x y z]]
  [
    [x y (+ z 1)]
    [x y (+ z -1)]
    [x (+ y 1) z]
    [x (+ y -1) z]
    [(+ x 1) y z]
    [(+ x -1) y z]
  ]
)

(defn in-bounds [bounds [x y z]]
  (let [[[x1 x2] [y1 y2] [z1 z2]] bounds]
    (case (and
           (<= (- x1 1) x (+ x2 1))
           (<= (- y1 1) y (+ y2 1))
           (<= (- z1 1) z (+ z2 1)))
      true :in-bounds
      false :out-of-bounds)))


(defrecord State [points bounds-check visited sum point])

(defn solve-aux [state]
  (let [{bounds-check :bounds-check points :points point :point} state]
    (case (contains? (:visited state) point)
      true (case (contains? points point)
             true (update state :sum + 1)
              false state)
      false (let [new-state (update state :visited conj point)]
        (case (bounds-check point)
          :out-of-bounds new-state
          :in-bounds (case (contains? points point)
            true (update new-state :sum + 1)
            false (reduce
                    (fn [acc-state cur-point] (solve-aux (assoc acc-state :point cur-point)))
                    new-state
                    (neighbors point))))))))
  

(defn solve [points]
  (let [state (solve-aux (State. points #(in-bounds (points-bounds points) %) #{} 0 [0 0 0])) ]
    (:sum state)))


;; ================ Main ================
(defn -main []
  (let [input_string (slurp "input.txt")]
    (print (format "Solution: %d\n" (solve (parse input_string))))))
(-main)
(ns p2
  (:require [clojure.set :as set]
            [clojure.string :as str])
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
    (and
           (<= (- x1 1) x (+ x2 1))
           (<= (- y1 1) y (+ y2 1))
           (<= (- z1 1) z (+ z2 1)))
  ))


(defrecord State [is-point bounds-check visited sum queue])

(defn should-check [is-point bounds-check visited point]
  (and
   (bounds-check point)
   (or 
    (not (contains? visited point))
    (is-point point))))

(defn solve-aux [state]
  (let [{bounds-check :bounds-check
         visited :visited
         sum :sum
         is-point :is-point
         queue :queue} state]
;;(println (pr-str queue))
;;(println (format "Queue: %d, Visited: %d, Sum: %d" (count queue) (count visited) sum))
(case queue
  [] state
  (let [new-visited (set/union visited (set queue))]
    (recur (State.
     is-point
     bounds-check
     new-visited
     (+ sum (count (filter is-point queue)))
     (filter #(should-check is-point bounds-check new-visited %) (apply concat (map neighbors (filter #(not (is-point %)) (set queue)))))))))))
     
  

(defn solve [points]
  (let [state (solve-aux (State. #(contains? points %) #(in-bounds (points-bounds points) %) #{} 0 [[0 0 0]] )) ]
    (:sum state)))


;; ================ Main ================
(defn -main []
  (let [input_string (slurp "input.txt")]
    (print (format "Solution: %d\n" (solve (parse input_string))))))
(-main)
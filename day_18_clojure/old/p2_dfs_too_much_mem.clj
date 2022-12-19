(ns p1.core
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
    (and
      (<= (- x1 1) x (+ x2 1))
      (<= (- y1 1) y (+ y2 1))
      (<= (- z1 1) z (+ z2 1)))))

(declare solve-aux)
(defn solve-aux-in-bounds-not-visited-not-point [points bounds visited point]
  (let [new-visited (conj visited point)]
    (reduce
      (fn [[sum visited] point]
        (let [[sum0 visited0] (solve-aux points bounds visited point)]
          [(+ sum sum0) visited0]))
      [0, new-visited]
      (neighbors point))))

(defn solve-aux-in-bounds-not-visited [points bounds visited point]
  (case (contains? points point)
    true [1 visited]
    false (solve-aux-in-bounds-not-visited-not-point points bounds visited point)))

(defn solve-aux-in-bounds [points bounds visited point]
  (case (contains? visited point)
    true [0 visited]
    false (solve-aux-in-bounds-not-visited points bounds visited point)))

(defn solve-aux [points bounds visited point]
  (case (in-bounds bounds point)
    false [0 visited]
    true (solve-aux-in-bounds points bounds visited point)))

(defn solve [points]
  (print (info-input points))
  (let [[[x1 x2] [y1 y2] [z1 z2]] (points-bounds points)]
    (let [[solution _] (solve-aux points (points-bounds points) #{} [(- x1 1) (- y1 1) (- z1 1)])]
      solution)))

;; ================ Main ================
(defn Main []
  (def input_string (slurp "input.txt"))
  (print (format "Solution: %d\n" (solve (parse input_string))))
)

(Main)
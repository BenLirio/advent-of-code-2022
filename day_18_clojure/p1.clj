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

(defn num-neighbors [points point]
  (count (filter #(contains? points %) (neighbors point))))


(defn solve [points]
  (print (info-input points))
  (-
    (* (count points) 6)
    (reduce + (map #(num-neighbors points %) points)))
)

;; ================ Main ================
(defn Main []
  (def input_string (slurp "input_custom.txt"))
  (print (format "Solution: %d\n" (solve (parse input_string))))
)

(Main)
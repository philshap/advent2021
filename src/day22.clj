(ns day22
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[toggle ranges] (str/split line #" ")
        [x1 x2 y1 y2 z1 z2] (map read-string (re-seq #"-?\d+" ranges))]
    [(sort [x1 x2]) (sort [y1 y2]) (sort [z1 z2]) (keyword toggle)]))

(def input
  (->> (slurp "src/day22-input.txt")
       str/split-lines
       (map parse-line)))

(defn in-range [pos region]
  (and (<= (first region) pos) (<= pos (last region))))

(defn maybe-toggle [pos current region]
  (if (every? #(in-range (nth pos %) (nth region %)) (range 3))
    (last region)
    current))

(defn part1 []
  (let [regions (take 20 input)]
    (->> (for [x (range -50 51)
               y (range -50 51)
               z (range -50 51)
               :let [pos [x y z]]]
           (reduce (partial maybe-toggle pos) :off regions))
         frequencies
         :on)))

(defn region-volume [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (- x2 x1) (- y2 y1) (- z2 z1)))

(defn overlap-axis [[a1-min a1-max] [a2-min a2-max]]
  (and (> a1-max a2-min) (< a1-min a2-max)))

(defn overlapping? [[x1 y1 z1] [x2 y2 z2]]
  (and (overlap-axis x1 x2)
       (overlap-axis y1 y2)
       (overlap-axis z1 z2)))

(defn handle-overlaps [r1 r2]
  (if (overlapping? r1 r2)))

(defn merge-regions [merged [xs ys zs toggle]]
  (if (= toggle :on)
    (conj merged [xs ys zs])
    merged))

(defn part2 []
  (->> (reduce merge-regions [] input)
       (map region-volume)
       (reduce +)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
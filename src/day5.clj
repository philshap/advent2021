(ns day5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day5-input.txt"
      slurp
      str/split-lines))

(defn range-either [v1 v2]
  (if (< v1 v2) (range v1 (inc v2)) (range v2 (inc v1))))

; maybe rewrite to avoid conditionals
(defn parse-line-1 [line]
  (let [[_ & points] (re-matches #"(\d+),(\d*) -> (\d+),(\d+)" line)
        [x1 y1 x2 y2] (map edn/read-string points)]
    (cond
      (= x1 x2) (map #(vector x1 %) (range-either y1 y2))
      (= y1 y2) (map #(vector % y1) (range-either x1 x2)))))

(defn count-overlaps [segments]
  (->> segments
       (map frequencies)
       (reduce (partial merge-with +))
       vals
       (remove #(= % 1))
       count))

(defn part1 []
  (->> input
       (map parse-line-1)
       count-overlaps))

; must be a better way
(defn parse-line-2 [line]
  (let [[_ & points] (re-matches #"(\d+),(\d*) -> (\d+),(\d+)" line)
        [x1 y1 x2 y2] (map edn/read-string points)
        dx (Integer/signum (- x2 x1))
        dy (Integer/signum (- y2 y1))]
    (loop [pos [x1 y1]
           points ()]
      (if (= pos [x2 y2])
        (conj points pos)
        (recur (mapv + [dx dy] pos) (conj points pos))))))

(defn part2 []
  (->> input
       (map parse-line-2)
       count-overlaps))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
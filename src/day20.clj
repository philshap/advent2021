(ns day20
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "src/day20-input.txt")
      (str/split #"\n\n")))

(defn parse-image [data]
  (let [lines (str/split-lines data)]
    (->>
      (for [y (range (count lines))
            :let [line (nth lines y)]
            x (range (count line))]
        [[x y] (nth line x)])
      (into {}))))

(def neighbors (for [y [-1 0 1], x [-1 0 1]] [x y]))

(defn pixel-neighbors [pixel]
  (map (partial map + pixel) neighbors))

(defn pixel-value [image default pixel]
  (reduce (fn [value neighbor]
            (+ (* value 2) (if (= \# (get image neighbor default)) 1 0)))
          0 (pixel-neighbors pixel)))

(defn enhance [algorithm image default pixel]
  (nth algorithm (pixel-value image default pixel)))

(defn find-possible-pixels [image]
  (let [xs (map first (keys image))
        ys (map second (keys image))]
    (for [x (range (dec (reduce min xs)) (+ 2 (reduce max xs)))
          y (range (dec (reduce min ys)) (+ 2 (reduce max ys)))]
      [x y])))

(defn step [[algorithm [default & next-default] image]]
  (->> (find-possible-pixels image)
       (map (juxt identity (partial enhance algorithm image default)))
       (into {})
       (vector algorithm next-default)))

(defn enhance-n-times [algorithm image times]
  (->> [algorithm (cycle [\. \#]) image]
       (iterate step)
       (drop times)
       first
       last
       vals
       frequencies
       (#(get % \#))))

(defn part1 []
  (let [[algorithm image-data] input]
    (enhance-n-times algorithm (parse-image image-data) 2)))

(defn part2 []
  (let [[algorithm image-data] input]
    (enhance-n-times algorithm (parse-image image-data) 50)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
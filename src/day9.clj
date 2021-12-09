(ns day9
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input
  (->> "src/day9-input.txt"
       slurp
       str/split-lines
       (mapv (comp #(mapv edn/read-string %) #(str/split % #"")))))

(def adj [[-1 0] [1 0] [0 -1] [0 1]])

(defn get-depth [[x y]]
  (let [row (nth input y nil)]
    (and row (nth row x nil))))

(defn neighbors [pos]
  (->> adj
       (map (comp get-depth #(mapv + pos %)))
       (remove nil?)))

(defn low-point? [pos]
  (every? (partial < (get-depth pos)) (neighbors pos)))

(defn find-low-points []
  (->> (for [y (range 0 (count input))
             x (range 0 (count (first input)))]
         [x y])
       (filter low-point?)))

(defn part1 []
  (->> (find-low-points)
       (map (comp inc get-depth))
       (reduce +)))

(defn basin-neighbors [pos]
  (->> adj
       (map #(mapv + pos %))
       (remove #(= 9 (or (get-depth %) 9)))))

(defn fill-basin [basin]
  (set/union basin (set (mapcat basin-neighbors basin))))

(defn basin-size [pos]
  (loop [basin (fill-basin #{pos})]
    (let [new-basin (fill-basin basin)]
      (if (= new-basin basin)
        (count basin)
        (recur new-basin)))))

(defn part2 []
  (->> (find-low-points)
       (map basin-size)
       sort
       reverse
       (take 3)
       (reduce *)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
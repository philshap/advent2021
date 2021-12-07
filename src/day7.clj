(ns day7
  (:require [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def input
  (->> "src/day7-input.txt"
       slurp
       (format "[%s]")
       edn/read-string))

(defn total-needed [crabs fuel-needed pos]
  (->> crabs
       (map (comp fuel-needed math/abs (partial - pos)))
       (reduce +)))

(defn find-crab-fuel [fuel-needed]
  (->> (range (reduce min input) (reduce max input))
       (map (partial total-needed input fuel-needed))
       (reduce min)))

(defn part1 []
  (find-crab-fuel identity))

(defn part2 []
  (find-crab-fuel #(quot (* % (inc %)) 2)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
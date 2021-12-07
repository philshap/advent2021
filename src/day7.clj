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
       sort
       first))

(defn part1 []
  (find-crab-fuel identity))

(defn fuel-needed [distance] (quot (* distance (inc distance)) 2))

(defn part2 []
  (find-crab-fuel fuel-needed))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
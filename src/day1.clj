(ns day1
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (->> "src/day1-input.txt"
       slurp
       str/split-lines
       (map edn/read-string)))

(defn count-increase [data]
  (->> data
       (partition 2 1)
       (filter #(apply < %))
       count))

(defn part1 []
  (count-increase input))

(defn part2 []
  (->> input
       (partition 3 1)
       (map #(reduce + %))
       count-increase))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
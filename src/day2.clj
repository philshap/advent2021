(ns day2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-line [line]
  (let [[_ dir amount] (re-matches #"(.*) (\d*)" line)]
    [dir (edn/read-string amount)]))

(def input
  (->> "src/day2-input.txt"
       slurp
       str/split-lines
       (map parse-line)))

(def dir->delta {"forward" [1 0], "up" [0 -1], "down" [0 1]})

(defn part1 []
  (->> input
       (reduce
         (fn [pos [dir delta]]
           (mapv + pos
                 (mapv (partial * delta)
                       (dir->delta dir))))
         [0 0])
       (reduce *)))

(defn part2 []
  (->> input
       (reduce
         (fn [pos [dir delta]]
           (mapv + pos
                 (case dir
                   "forward" [delta (* delta (last pos)) 0]
                   "up" [0 0 (- delta)]
                   "down" [0 0 delta])))
         [0 0 0])
       (#(* (first %) (second %))))
  )

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
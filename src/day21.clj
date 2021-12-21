(ns day21
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> (slurp "src/day21-input.txt")
       str/split-lines
       (map #(read-string (last (str/split % #" "))))))

(defn next-pos [pos roll-total]
  (inc (mod (dec (+ pos roll-total)) 10)))

(defn part1 []
  (let [[p1-start p2-start] input]
    (loop [[pos score] [p1-start 0]
           [_ lose-score :as player2] [p2-start 0]
           roll (cycle (range 1 101))
           turn 0]
      (let [new-pos (next-pos pos (reduce + (take 3 roll)))
            new-score (+ score new-pos)]
        (if (>= new-score 1000)
          (* (* 3 (inc turn)) lose-score)
          (recur player2 [new-pos new-score] (drop 3 roll) (inc turn)))))))

(def common-rolls
  (->> (combo/selections [1 2 3] 3)
       (map (partial reduce +))
       (frequencies)))

(def play-game
  (memoize
    (fn [[pos score id] player2]
      (->> (for [[roll-total frequency] common-rolls]
             (let [new-pos (next-pos pos roll-total)
                   new-score (+ score new-pos)]
               (if (>= new-score 21)
                 {id frequency}
                 (->> (play-game player2 [new-pos new-score id])
                      (map (fn [[k v]] [k (* v frequency)]))
                      (into {})))))
           (reduce (partial merge-with +))))))

(defn part2 []
  (let [[p1-start p2-start] input]
    (->> (play-game [p1-start 0 :p1] [p2-start 0 :p2])
         vals
         (reduce max))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
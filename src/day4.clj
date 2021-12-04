(ns day4
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input
  (-> "src/day4-input.txt"
      slurp
      (str/split #"\n\n")))

(defn read-balls [line]
  (vec (map edn/read-string (str/split line #","))))

; A bingo board is a list of sets. Each set is a row or column on the board.
(defn read-board [data]
  (->>
    ; Generate a list of maps. Each map contains a row and col ID and a set of the tile for that row or col.
    (let [lines (->> data str/split-lines (map #(str/split (str/trim %) #"\s+")))]
      (for [y (range (count lines))
            :let [line (nth lines y)]
            x (range (count line))
            :let [tile #{(edn/read-string (nth line x))}]]
        {(str "row" y) tile, (str "col" x) tile}))
    ; Merge based on the row or col ID. The merged data is a union of the tile numbers.
    (reduce (partial merge-with set/union))
    ; After merging, discard the row and col IDs. The result is a list of sets.
    vals))

(defn bingo? [board balls]
  (->> board
       (filter (partial set/superset? balls))
       first))

(defn board-score [board balls just-called]
  (* just-called (reduce + (set/difference (reduce set/union board) balls))))

(defn play-bingo [balls board]
  (loop [turn 1]
    (let [drawn (set (subvec balls 0 turn))]
      (if (bingo? board drawn)
        {:turn turn, :score (board-score board drawn (nth balls (dec turn)))}
        (recur (inc turn))))))

(defn play-all-boards []
  (let [balls (read-balls (first input))
        boards (map read-board (rest input))]
    (->> boards
         (map (partial play-bingo balls))
         (sort-by :turn))))

(defn part1 []
  (-> (play-all-boards) first :score))

(defn part2 []
  (-> (play-all-boards) last :score))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
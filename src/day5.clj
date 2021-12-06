(ns day5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day5-input.txt"
      slurp
      str/split-lines))

(defn parse-line [line]
  (let [[x1 y1 _ x2 y2] (edn/read-string (format "[%s]" line))]
    [[x1 y1] [x2 y2]]))

(defn diagonal? [[start end]]
  (every? true? (mapv not= start end)))

(defn draw-line [[start end]]
  (let [delta (mapv (comp #(Integer/signum %) -) end start)]
    (->> (iterate (partial mapv + delta) start)
         (take-while (partial not= (mapv + delta end))))))

(defn count-intersections [maybe-filter]
  (->> input
       (map parse-line)
       maybe-filter
       (mapcat draw-line)
       frequencies
       vals
       (remove (partial = 1))
       count))

(defn part1 []
  (count-intersections (partial remove diagonal?)))

(defn part2 []
  (count-intersections identity))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
(ns day14
  (:require [clojure.string :as str]))

(def input
  (-> "src/day14-input.txt"
      slurp
      (str/split #"\n\n")))

(defn parse-rules [input]
  (->> (second input)
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [[[A B] [C]]] [[A B] C]))
       (into {})))

;; a polymer is stored as a frequency map of element pairs to the count of the pair in the polymer
(defn apply-rules [rules polymer]
  (->> polymer
       (map (fn [[[A B :as pair] total]]
              (let [insert (rules pair)]
                {[A insert] total
                 [insert B] total})))
       (reduce (partial merge-with +))))

(defn compute-score [polymer-map]
  (->> polymer-map
       (map (fn [[poly total]] {(first poly) total}))
       (reduce (partial merge-with +))
       vals
       (#(- (reduce max %) (reduce min %)))))

(defn expand-polymer [input limit]
  (let [template (first input)
        rules (parse-rules input)]
    (->> (partition 2 1 template)
         frequencies
         (iterate (partial apply-rules rules))
         (drop limit)
         first
         ; compute-score won't include the last element, so we add it here
         (#(conj % {[(last template)] 1}))
         compute-score)))

(defn part1 []
  (expand-polymer input 10))

(defn part2 []
  (expand-polymer input 40))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
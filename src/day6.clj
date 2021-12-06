(ns day6
  (:require [clojure.edn :as edn]))

(def input
  (->>
    "src/day6-input.txt"
       slurp
       (format "[%s]")
       edn/read-string))

;; group: (count -> fish age)
;; school: (group-7, group-9)

(defn spawn [group from-cycle to-cycle tick]
  ;; find the number of fish whose age would be 0 at this tick, and spawn them
  {(mod tick to-cycle) (group (mod tick from-cycle) 0)})

(defn breed-fish [num-generations]
  (loop [group7 (frequencies input)
         group9 {}
         generation 0]
    (if (= generation num-generations)
      (+ (reduce + (vals group7)) (reduce + (vals group9)))
      (recur
        (merge-with + group7 (spawn group9 9 7 generation))
        (merge-with + group9 (spawn group7 7 9 generation))
        (inc generation)))))

(defn part1 []
  (breed-fish 80))

(defn part2 []
  (breed-fish 256))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
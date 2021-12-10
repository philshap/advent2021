(ns day10
  (:require [clojure.string :as str]))

(def input
  (-> "src/input"
      slurp
      str/split-lines))

(def open->close {\{ \}, \( \), \[ \], \< \>})
(def close->points {\) 3, \] 57, \} 1197, \> 25137})

;; return remaining bracket stack, or corrupt bracket
(defn parse-line [line]
  (reduce
    (fn [stack bracket]
      (if-some [close (open->close bracket)]
        (conj stack close)
        (if (= bracket (peek stack))
          (pop stack)
          ; return corrupt bracket
          (reduced bracket))))
    () line))

(defn part1 []
  (->> input
       (map parse-line)
       (remove seq?)
       (map close->points)
       (reduce +)))

(def close->points2 {\) 1, \] 2, \} 3, \> 4})

(defn complete-score [stack]
  (reduce
    #(+ (* %1 5) (close->points2 %2))
    0 stack))

(defn part2 []
  (->> input
       (map parse-line)
       (filter seq?)
       (map complete-score)
       sort
       (#(nth % (quot (count %) 2)))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
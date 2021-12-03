(ns day3
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map #(- (int %) (int \0)) line))

(def input
  (->> "src/day3-input.txt"
       slurp
       str/split-lines
       (map parse-line)))

(defn part1 []
  (->> input
       (reduce (partial mapv +))
       (reduce (fn [result sum]
                 (+ (bit-shift-left result 1)
                    (if (> sum (/ (count input) 2)) 1 0))) 0)
       (#(* % (bit-and (dec (bit-shift-left 1 (count (first input))))
                       (bit-not %))))))

(defn filter-ratings [f pos ratings]
  (let [total (->> ratings
                   (map #(nth % pos))
                   flatten
                   (reduce +))]
    (filter (fn [rating]
              (if (f total (/ (count ratings) 2))
                (= (nth rating pos) 1)
                (= (nth rating pos) 0)))
            ratings)))

(defn rating->value [rating]
  (reduce (fn [result digit] (+ (bit-shift-left result 1) digit)) 0 rating))

(defn find-rating [f]
  (loop [pos 0
         ratings input]
    (if (= 1 (count ratings))
      (rating->value (first ratings))
      (recur (inc pos)
             (filter-ratings f pos ratings)))))

(defn part2 []
  (* (find-rating >=) (find-rating <)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
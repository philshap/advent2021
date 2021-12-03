(ns day3
  (:require [clojure.string :as str]))

(def input
  (-> "src/day3-input.txt"
      slurp
      str/split-lines))

(defn parse-line [line]
  (->> line
       vec
       (map #(- (int %) (int \0)))))

(defn part1 []
  (->> input
       (map parse-line)
       (reduce (partial mapv +))
       (reduce (fn [result sum]
                 (+ (bit-shift-left result 1)
                    (if (> sum (/ (count input) 2)) 1 0))) 0)
       (#(* % (bit-and (dec (bit-shift-left 1 (count (first input))))
                       (bit-not %))))))

(defn rating-filter [f pos ratings]
  (let [total (->> ratings
                   (map #(nth % pos))
                   flatten
                   (reduce +))]
    (filter (fn [rating]
              (if (f total (/ (count ratings) 2))
                (= (nth rating pos) 1)
                (= (nth rating pos) 0)))
            ratings)))

(defn o2-rating [pos ratings]
  (rating-filter >= pos ratings))

(defn co2-rating [pos ratings]
  (rating-filter < pos ratings))

(defn find-rating [f]
  (loop [pos 0
         ratings (map parse-line input)]
    (if (= 1 (count ratings))
      (first ratings)
      (recur (inc pos)
             (f pos ratings)))))

(defn rating->value [v]
  (reduce (fn [result digit] (+ (bit-shift-left result 1) digit)) 0 v))

(defn part2 []
  (* (rating->value (find-rating o2-rating)) (rating->value (find-rating co2-rating))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
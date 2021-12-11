(ns day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day8-input.txt"
      slurp
      str/split-lines))

(defn parse-line [line]
  (-> line (str/split #"\|")
      (->> (map str/trim) (map #(str/split % #" ")))))

;;  aaa
;; b   c
;; b   c
;;  ddd
;; e   f
;; e   f
;;  ggg
(def segments ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"])
(def unique? (->> segments (map count) frequencies (filter #(= (second %) 1)) keys (into #{})))

(defn part1 []
  (->> input
       (mapcat (comp (partial map count) last parse-line))
       (filter unique?)
       count))

(defn segment-frequencies [digits] (frequencies (reduce concat digits)))
(defn freq-total [digits]
  (map #(reduce + (map segment-frequencies %)) digits))
(def freq-total->number (zipmap (map freq-total segments) (range)))

(defn decode [digit all-digits]
  (let [one (first (filter #(= (count %) 2) all-digits))
        four (first (filter #(= (count %) 4) all-digits))
        one-overlaps (fn [digit] (count (set/intersection one digit)))
        four-overlaps (fn [digit] (count (set/intersection four digit)))]
    (case (count digit)
     ; 1 - 2 segments
     2 1
     ; 7 - 3 segments
     3 7
     ; 4 - 4 segments
     4 4
     ; 2 - 5 segments, one overlap with 1, two overlaps with 4
     ; 3 - 5 segments, two overlaps with 1
     ; 5 - 5 segments, one overlap with 1, three overlaps with 4
     5 (if (= (one-overlaps digit) 2)
         3
         (if (= (four-overlaps digit) 2) 2 5))
     ; 0 - 6 segments, two overlaps with 1, three overlaps with 4
     ; 6 - 6 segments, one overlap with 1, three overlaps with 4
     ; 9 - 6 segments, two overlaps with 1, four overlaps with 4
     6 (if (= (one-overlaps digit) 1)
         6
         (if (= (four-overlaps digit) 4) 9 0))
     ; 8 - 7 segments
     7 8)))

(defn decode-line [line]
  (let [code (map set (last line))
        all-digits (map set (first line))]
    (reduce (fn [output digit]
              (+ (* 10 output) (decode digit all-digits)))
            0 code)))

(defn part2 []
  (->> input
       (map parse-line)
       (map decode-line)
       (reduce +)))

;(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  ;)
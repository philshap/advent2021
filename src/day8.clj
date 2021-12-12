(ns day8
  (:require [clojure.string :as str]))

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
;; segments is the digits 0 - 9 represented as the segments lit up when the digit is shown.
(def segments ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"])
(def unique? (->> segments (map count) frequencies (filter #(= (second %) 1)) keys (into #{})))

(defn part1 []
  (->> input
       (mapcat (comp (partial map count) last parse-line))
       (filter unique?)
       count))

(defn create-ref-frequencies [digits] (frequencies (reduce concat digits)))
(def segment-frequencies (create-ref-frequencies segments))
(defn freq-total [ref-frequencies digits] (reduce + (map ref-frequencies digits)))
(def freq-total->number (zipmap (map (partial freq-total segment-frequencies) segments) (range)))

(defn decode-line [[all-digits code]]
  (let [ref-frequencies (create-ref-frequencies all-digits)]
    (reduce (fn [output digit]
              (+ (* 10 output) (freq-total->number (freq-total ref-frequencies digit))))
            0 code)))

(defn part2 []
  (->> input
       (map (comp decode-line parse-line))
       (reduce +)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
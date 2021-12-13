(ns day13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day13-input.txt"
      slurp
      (str/split #"\n\n")))

(defn read-dots [raw-dots]
  (->> raw-dots
       str/split-lines
       (map (comp edn/read-string (partial format "[%s]")))
       (into #{})))

(defn apply-fold [dots fold]
  (let [[inst raw-value] (str/split fold #"=")
        value (edn/read-string raw-value)
        flip #(- (* value 2) %)]
    (if (= \x (last inst))
      (into #{} (map #(if (< (first %) value) % [(flip (first %)) (second %)]) dots))
      (into #{} (map #(if (< (second %) value) % [(first %) (flip (second %))]) dots)))))

(defn part1 []
  (let [dots (read-dots (first input))
        folds (str/split-lines (second input))]
    (count (apply-fold dots (first folds)))))

(defn draw [dots]
  (dorun
    (let [max-x (reduce max (map first dots))]
      (for [y (range (inc (reduce max (map second dots))))
            x (range (inc max-x))]
        (do (print (if (dots [x y]) "##" ".."))
            (when (= x max-x) (println)))))))

(defn part2 []
  (let [dots (read-dots (first input))
        folds (str/split-lines (second input))]
    (draw (reduce apply-fold dots folds))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
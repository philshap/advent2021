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
       (map (comp edn/read-string (partial format "[%s]")))))

(defn apply-fold [dots fold]
  (let [[inst raw-value] (str/split fold #"=")
        value (edn/read-string raw-value)
        flip #(- (* value 2) %)]
    (into #{} (map
                (fn [[x y :as dot]]
                  (if (= \x (last inst))
                    (if (< x value) dot [(flip x) y])
                    (if (< y value) dot [x (flip y)])))
                dots))))

(defn part1 []
  (let [dots (read-dots (first input))
        folds (str/split-lines (second input))]
    (count (apply-fold dots (first folds)))))

(defn draw [dots]
  (reduce str (for [y (range (inc (reduce max (map second dots))))
                    x (range (inc (reduce max (map first dots))))]
                (str (if (zero? x) "\n" "")
                     (if (dots [x y]) "##" "..")))))

(defn part2 []
  (let [dots (read-dots (first input))
        folds (str/split-lines (second input))]
    (draw (reduce apply-fold dots folds))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
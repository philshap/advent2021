(ns day25
  (:require [clojure.string :as str]))

(def input
  (-> "src/day25-input.txt"
      slurp
      str/split-lines))

(def width (count (first input)))
(def height (count input))

(defn read-trench [input]
  (into {}
        (for [x (range width), y (range height)
              :let [tile (nth (nth input y) x)]
              :when (not= tile \.)]
          [[x y] tile])))

(comment
  (defn draw [trench]
    (dorun (for [y (range height)
                 x (range width)]
             (do (print (get trench [x y] \.))
                 (when (= x (dec width)) (println)))))))

(def cucumber->delta {\v [0 1], \> [1 0]})

(defn move-cucumber [cucumber pos]
  (->>
    (map + (cucumber->delta cucumber) pos)
    ((fn [[x y]] [pos [(mod x width) (mod y height)]]))))

(defn move-cucumbers [trench cucumber]
  (->> (filter #(= (trench %) cucumber) (keys trench))
       (map (partial move-cucumber cucumber))
       (filter #(nil? (trench (second %))))))

(defn update-trench [trench cucumber]
  (let [moved (move-cucumbers trench cucumber)
        removed (apply dissoc trench (map first moved))]
    (reduce (fn [new-trench new-pos] (assoc new-trench new-pos cucumber))
            removed (map second moved))))

(defn step [[trench turn]]
  (let [trench (update-trench trench \>)
        trench (update-trench trench \v)]
    [trench (inc turn)]))

(defn part1 []
  (->> [(read-trench input) 0]
       (iterate step)
       (partition 2)
       (drop-while #(not= (ffirst %) (first (second %))))
       ffirst
       second))

(defn part2 []
  "Nothing to do!")

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
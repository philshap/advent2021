(ns day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  (defn draw [levels]
    (dorun
      (for [y (range 10)
            x (range 10)
            :let [level (levels [x y])]]
        (do (print (if (nil? level) \. (if (> level 9) \X level)))
            (when (= x 9) (println)))))))

(def input
  (-> "src/day11-input.txt"
      slurp
      str/split-lines))

(defn parse-input [input]
  (into {}
        (for [y (range (count input))
              x (range (count (first input)))]
          [[x y] (- (int (nth (nth input y) x)) (int \0))])))

(defn raise-energy [levels]
  (into {} (for [[k v] levels] [k (inc v)])))

(defn find-flash [levels]
  (->> levels (filter (fn [[_ v]] (> v 9))) keys))

(def adj (for [x (range -1 2)
               y (range -1 2)
               :when (not= 0 x y)]
           [x y]))

(defn flash-adjacent [flashed]
  (mapcat #(map (partial mapv + %) adj) flashed))

(defn raise-adjacent [flashed levels]
  (reduce (fn [levels pos]
            (if-let [old (levels pos nil)]
              (assoc levels pos (inc old))
              levels))
          levels (flash-adjacent flashed)))

(defn remove-flashed [flashed levels]
  (into {} (remove (fn [[k _]] (flashed k)) levels)))

(defn next-cycle [[levels flash-count]]
  (loop [new-levels (raise-energy levels)
         all-flashed #{}]
    (let [flashed (find-flash new-levels)
          set-flashed (set flashed)]
      (if (empty? flashed)
        [(merge new-levels (reduce merge (for [pos all-flashed] {pos 0})))
         (+ flash-count (count all-flashed))]
        (recur (raise-adjacent flashed (remove-flashed set-flashed new-levels))
               (set/union all-flashed set-flashed))))))

(defn flash-stream [levels]
  (->> [levels 0]
       (iterate next-cycle)
       (map second)))

(defn part1 []
  (->> (flash-stream (parse-input input))
       (drop 100)
       first))

(defn part2 []
  (let [levels (parse-input input)]
    (->> (flash-stream levels)
         (partition 2 1)
         (map (fn [[x1 x2]] (- x2 x1)))
         (take-while (partial not= (count levels)))
         count
         inc)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
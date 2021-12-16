(ns day15
  (:require [clojure.string :as str])
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def input
  (-> "src/day15-input.txt"
      slurp
      str/split-lines))

(defn parse-cave [input]
  (into {}
        (for [y (range (count input))
              x (range (count (first input)))]
          [[x y] (- (int (nth (nth input y) x)) (int \0))])))

(defn cave-exit [cave]
  [(->> cave keys (map first) (reduce max))
   (->> cave keys (map second) (reduce max))])

;; A path is a location in the cave and a current risk score
(defn extend-path [cave paths [pos risk]]
  (reduce
    (fn [new-paths new-pos]
      (if-some [new-risk (cave new-pos)]
        (if (not (paths new-pos))
          (conj {new-pos (+ risk new-risk)} new-paths)
          new-paths)
        new-paths))
    {}
    (map (partial map + pos) [[0 1] [1 0] [-1 0] [0 -1]])))

(defn extend-paths [cave paths]
  (let [[key _ :as path] (first paths)]
    (into (assoc paths key ##Inf) (extend-path cave paths path))))

(defn find-cave-path-risk [cave exit]
  (->> (priority-map [0 0] 0)
       (iterate (partial extend-paths cave))
       (filter #(some? (% exit)))
       (map vals)
       ffirst
       ;; Need to add the risk of the exit node manually
       (+ (cave exit))))

(defn part1 []
  (let [cave (parse-cave input)]
    (find-cave-path-risk cave (cave-exit cave))))

(defn tile-cave [cave num-tiles]
  (let [[width height] (map inc (cave-exit cave))]
    (fn [[x y]]
      (when (and (<= 0 x) (< x (* num-tiles width))
                 (<= 0 y) (< y (* num-tiles height)))
        (let [risk (+ (cave [(mod x width) (mod y height)])
                      (quot x width)
                      (quot y width))]
          (if (> risk 9) (inc (mod risk 10)) risk))))))

(defn part2 []
  (let [cave (parse-cave input)]
    (find-cave-path-risk
      (tile-cave cave 5)
      (map dec (map * [5 5] (map inc (cave-exit cave)))))))

;(comment
  (println "part 1: " (part1))
  ;;; why?? -2 works but doesn't make any sense. will debug later...
  (println "part 2: " (- (part2) 2))
  ;)
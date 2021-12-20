(ns day19
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]))

(defn parse-scanner [data]
  (->> data
       str/split-lines
       rest
       (map (comp read-string #(format "(%s)" %)))))

(def scanners
  (->> "src/day19-input.txt"
       slurp
       (#(str/split % #"\n\n"))
       (map parse-scanner)))

(defn beacon-set [placed]
  (into #{} (reduce concat placed)))

(def dirs [-1 1])
(def rotations
  (for [x dirs, y dirs, z dirs]
    [x y z]))

(defn apply-rotation [locations rotation]
  (map (partial map * rotation) locations))

(defn permute-place [beacons perm]
  (map (fn [[x y z]]
         (case perm
           0 (list x y z)
           1 (list x z y)
           2 (list y x z)
           3 (list y z x)
           4 (list z x y)
           5 (list z y x)))
       beacons))

(defn apply-permutes [beacons]
  (map (partial permute-place beacons) (range 6)))

(defn shift-place [beacons offset]
  [(map (partial map + offset) beacons) offset])

(defn apply-shift [placed-beacons try-beacons]
  (for [placed placed-beacons
        unplaced try-beacons]
    (shift-place try-beacons (map - placed unplaced))))

(defn place-scanner [placed try-place]
  (let [beacons (beacon-set placed)]
    (->> rotations
         (map (partial apply-rotation try-place))
         (mapcat apply-permutes)
         (mapcat (partial apply-shift beacons))
         (filter #(<= 12 (count (set/intersection beacons (into #{} (first %))))))
         first)))

(defn place-scanners [scanners]
  ;; Placed scanners are a list of beacons at the scanner and its position relative to the
  ;; first placed scanner. Scanner beacons are all stored relative to the first placed scanner.
  (loop [placed-scanners [[(first scanners) [0 0 0]]]
         to-place (rest scanners)]
    (println (count to-place))
    (if (empty? to-place)
      placed-scanners
      (let [try-place (first to-place)]
        (if-some [scanner (place-scanner (map first placed-scanners) try-place)]
          (recur (conj placed-scanners scanner) (rest to-place))
          (recur placed-scanners (concat (rest to-place) (list try-place))))))))

(defn part1 []
  (->> (place-scanners scanners)
       (map first)
       beacon-set
       count))

(defn manhattan [[[x1 y1 z1] [x2 y2 z2]]]
  (+ (math/abs (- x1 x2))
     (math/abs (- y1 y2))
     (math/abs (- z1 z2))))

(defn max-manhattan [offsets]
  (->> (combo/selections offsets 2)
       (map (partial manhattan))
       (reduce max)))

(defn part2 []
  (->> (place-scanners scanners)
       (map second)
       max-manhattan))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
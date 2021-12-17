(ns day17
  (:require [clojure.edn :as edn]))

(def input
  (->> (slurp "src/day17-input.txt")
       (re-seq #"-?\d+")
       (map edn/read-string)
       (partition 2)))

; HACK - ranges were manually determined after looking at the input.
; Should use a formula to derive these instead.
(defn generate-velocities []
  (for [x (range 10 300)
        y (range -150 200)]
    [x y]))

;The probe's x position increases by its x velocity.
;The probe's y position increases by its y velocity.
;Due to drag, the probe's x velocity changes by 1 toward the value 0;
; that is, it decreases by 1 if it is greater than 0,
;             increases by 1 if it is less than 0,
;             or does not change if it is already 0.
;Due to gravity, the probe's y velocity decreases by 1.
(defn step [[[x y] [dx dy] max-y]]
  (let [new-y (+ y dy)]
    [[(+ x dx) new-y] [(max 0 (dec dx)) (dec dy)] (max max-y new-y)]))

(defn missed? [[x-range y-range] [[x y] _ _]]
  (or (> x (reduce max x-range))
      (< y (reduce min y-range))))

(defn in-range [v [r1 r2]]
  (and (>= v (min r1 r2)) (<= v (max r1 r2))))

(defn hit? [[x-range y-range] [[x y] _ max-y]]
  (when (and (in-range x x-range) (in-range y y-range))
    max-y))

; returns the max y value in the trajectory if this velocity hits the target
(defn trajectory-hit [target velocity]
  (->> [[0 0] velocity 0]
       (iterate step)
       (take-while (complement (partial missed? target)))
       (map (partial hit? target))
       (remove nil?)
       first))

(defn find-hits [target]
  (->> (generate-velocities)
       (map (partial trajectory-hit target))
       (remove nil?)))

(defn part1 []
  (reduce max (find-hits input)))

(defn part2 []
  (count (find-hits input)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
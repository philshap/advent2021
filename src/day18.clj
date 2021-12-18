(ns day18
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.zip :as zip]))

(def input
  (->> (slurp "src/day18-input.txt")
       str/split-lines
       (map (comp zip/vector-zip read-string))))

(defn add-prev [val sn]
  (if-some [loc (zip/prev sn)]
    (if (zip/branch? loc)
      (add-prev val loc)
      (zip/edit loc (partial + val)))
    sn))

(defn prev-node [sn]
  (if (zip/branch? sn)
    (if-some [loc (zip/prev sn)]
      (prev-node loc)
      sn)
    sn))

(defn add-next [val sn]
  (let [loc (zip/next sn)]
    (cond
      (zip/end? loc) sn
      (zip/branch? loc) (recur val loc)
      :else (prev-node (zip/prev (zip/edit loc (partial + val)))))))

(defn find-root [sn]
  (if-some [up (zip/up sn)]
    (recur up)
    sn))

;; messy but it works
(defn explode [root]
  (loop [loc root]
    (cond
      (zip/end? loc) root
      (and (= 4 (count (zip/path loc)))
           (zip/branch? loc))
      (let [[l r] (zip/node loc)]
        (->> (zip/replace loc 0)
             (add-next r)
             (add-prev l)
             find-root))
      :else (recur (zip/next loc)))))

(defn split [root]
  (loop [loc root]
    (cond
      (zip/end? loc) root
      (zip/branch? loc) (recur (zip/next loc))
      :else
      (let [value (zip/node loc)]
        (if (> value 9)
          (let [div-2 (quot value 2)]
            (->> [div-2 (if (zero? (rem value 2)) div-2 (inc div-2))]
                 (zip/replace loc)
                 find-root))
          (recur (zip/next loc)))))))

(defn step [snail-number]
  (let [exploded (explode snail-number)]
    (if (not= snail-number exploded)
      exploded
      (split snail-number))))

(defn reduce-completely [sn]
  (->> sn
       (iterate step)
       (partition 2 1)
       (drop-while (partial apply not=))
       ffirst))

(defn add [sn1 sn2]
  (zip/vector-zip [(zip/root sn1) (zip/root sn2)]))

(defn add-and-reduce [sn1 sn2]
  (reduce-completely (add sn1 sn2)))

(defn magnitude [sn]
  (if (zip/branch? sn)
    (+ (* 3 (magnitude (zip/down sn)))
       (* 2 (magnitude (zip/right (zip/down sn)))))
    (zip/node sn)))

(defn part1 []
  (->> input
       (reduce add-and-reduce)
       magnitude))

(defn part2 []
  (->> (combo/combinations input 2)
       (map (partial apply add-and-reduce))
       (map magnitude)
       (reduce max)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
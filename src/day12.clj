(ns day12
  (:require [clojure.string :as str]))

(def input
  (-> "src/day12-input.txt"
      slurp
      str/split-lines))

(defn parse-edge [edge]
  (let [[node1 node2] (str/split edge #"-")]
    (cond
      (or (= node1 "start") (= node2 "end")) {node1 [node2]}
      (or (= node2 "start") (= node1 "end")) {node2 [node1]}
      :else {node1 [node2], node2 [node1]})))

(defn build-adj [input]
  (->> input
       (map parse-edge)
       (reduce (partial merge-with concat))))

(defn extend-path [node-filter adj path]
  (->> (adj (last path))
       (remove (partial node-filter path))
       (map (partial conj path))))

(defn complete? [path] (= "end" (last path)))

(defn extend-paths [node-filter adj paths]
  (concat
    (filter complete? paths)
    (mapcat (partial extend-path node-filter adj) paths)))

;; part 2

(defn visit-once? [node] (= node (str/lower-case node)))

(defn part1-filter [path node]
  (and (visit-once? node) (some (partial = node) path)))

(defn count-paths [node-filter adj]
  (->> [["start"]]
       (iterate (partial extend-paths node-filter adj))
       (drop-while (partial some (complement complete?)))
       first
       count))

(defn part1 []
  (count-paths part1-filter (build-adj input)))

;; part 2

(defn any-twice? [path]
  (->> (filter visit-once? path)
       frequencies
       vals
       (some (partial < 1))))

;; move any-twice? into this method and use frequencies result for `found`
(defn part2-filter [path node]
  (let [found (count (filter (partial = node) path))]
    (and (visit-once? node)
         (or (= found 2)
             (and (= found 1)
                  (any-twice? (remove #(= node %) path)))))))

(defn part2 []
  (count-paths part2-filter (build-adj input)))

; (part1)
;=> 5958
;(part2)
;=> 150426

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
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

(defn visit-once? [node] (= node (str/lower-case node)))

(defn part1-filter [path node]
  (and (visit-once? node) (some (partial = node) path)))

(defn extend-path [node-filter adj path]
  (->> (adj (last path))
       (remove (partial node-filter path))
       (map (partial conj path))))

(defn complete? [path] (= "end" (last path)))

;; could change to use `iterate` by filtering incomplete and then use
;; (->> [["start"]]
;;   (iterate (partial find-paths part1-filter (build-adj input))
;;   (drop-while (partial (some? (complement complete?)))
;;   first count)
(defn find-paths [node-filter adj]
  (loop [incomplete [["start"]]
         complete []]
    (let [new-paths (mapcat (partial extend-path node-filter adj) incomplete)]
      (if (empty? new-paths)
        complete
        (recur (remove complete? new-paths)
               (concat (filter complete? new-paths) complete))))))

(defn part1 []
  (->> input
       build-adj
       (find-paths part1-filter)
       count))

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
  (->> input
       build-adj
       (find-paths part2-filter)
       count))

; (part1)
;=> 5958
;(part2)
;=> 150426

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
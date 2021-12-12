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
       (apply (partial merge-with concat))))

(defn visit-once? [node] (= node (str/lower-case node)))

(defn extend-path [adj path]
  (->> (adj (last path))
       (remove #(and (visit-once? %) (some (partial = %) path)))
       (map (partial conj path))))

(defn complete? [path] (= "end" (last path)))

(defn find-paths [adj]
  (loop [paths [["start"]]]
    (let [new-paths (mapcat (partial extend-path adj) paths)]
      (if (empty? new-paths)
        paths
        (recur (concat (filter complete? paths) new-paths))))))

(defn part1 []
  (-> input
      build-adj
      find-paths
      count))

(defn any-twice? [path]
  (->> (filter visit-once? path)
       frequencies
       vals
       (some (partial < 1))))

(defn allow-twice [path node]
  (let [found (count (filter (partial = node) path))]
    (if (visit-once? node)
      (or (zero? found)
          (and (= found 1)
               (not (any-twice? (remove #(= node %) path)))))
      true)))

(defn extend-path-2 [adj path]
  (->> (adj (last path))
       (filter (partial allow-twice path))
       (map (partial conj path))))

(defn find-paths-2 [adj]
  (loop [paths [["start"]]]
    (let [new-paths (mapcat (partial extend-path-2 adj) paths)]
      (if (empty? new-paths)
        paths
        (recur (concat (filter complete? paths) new-paths))))))

(defn part2 []
  (-> input
      build-adj
      find-paths-2
      count))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
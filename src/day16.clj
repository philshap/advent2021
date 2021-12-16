(ns day16
  (:require [clojure.edn :as edn]))

(def input (slurp "src/day16-input.txt"))

;; stateful BITS api
(def bits-data (ref []))
(def bits-pos (ref 0))

(defn hex->bin [hex]
  (clojure.pprint/cl-format nil "~4,'0b" (edn/read-string (str "0x" hex))))

(defn expand-bits [data]
  (into [] (mapcat hex->bin data)))

(defn decode-bits [bits]
  (edn/read-string (str "2r" (apply str bits))))

(defn init-bits! [data]
  (dosync
    (ref-set bits-data (expand-bits data))
    (ref-set bits-pos 0)))

(defn read-bits! [n]
  (let [pos @bits-pos
        end (+ pos n)
        decoded (decode-bits (subvec @bits-data pos end))]
    (dosync (ref-set bits-pos end))
    decoded))

(defn read-pos []
  (deref bits-pos))

;; packet decoding
(defn decode-literal []
  (loop [value 0]
    (let [flag (read-bits! 1)
          data (read-bits! 4)
          new-value (+ (* value 16) data)]
      (case flag
        0 new-value
        1 (recur new-value)))))

(declare decode-packet)

(defn decode-subs []
  (case (read-bits! 1)
    0 (let [length (read-bits! 15)
            end-pos (+ (read-pos) length)]
        (loop [subs []]
          (if (= (read-pos) end-pos)
            subs
            (recur (conj subs (decode-packet))))))
    1 (loop [length (read-bits! 11)
             subs []]
        (if (zero? length)
          subs
          (recur (dec length) (conj subs (decode-packet)))))))

(defn decode-packet []
  (let [version (read-bits! 3), tag (read-bits! 3)]
    (case tag
      4 {:version version, :tag tag, :value (decode-literal)}
      {:version version :tag tag :subs (decode-subs)})))

(defn version-total [packet]
  (+ (:version packet)
     (reduce + (map version-total (:subs packet)))))

(defn part1 []
  (init-bits! input)
  (version-total (decode-packet)))

(defn eval-packet [packet]
  (let [args (map eval-packet (:subs packet))]
    (case (:tag packet)
      0 (reduce + args)
      1 (reduce * args)
      2 (reduce min args)
      3 (reduce max args)
      4 (:value packet)
      5 (if (> (first args) (second args)) 1 0)
      6 (if (< (first args) (second args)) 1 0)
      7 (if (= (first args) (second args)) 1 0))))

(defn part2 []
  (init-bits! input)
  (eval-packet (decode-packet)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
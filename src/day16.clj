(ns day16
  (:require [clojure.edn :as edn]))

(def input (slurp "src/day16-input.txt"))

;; BITS api

(defn hex->bin [hex]
  (clojure.pprint/cl-format nil "~4,'0b" (edn/read-string (str "0x" hex))))

(defn parse-input [data]
  (reduce str (mapcat hex->bin data)))

;; read `length` bits as an int, return the remaining bits
(defn read-bits [bits length]
  [(edn/read-string (str "2r" (subs bits 0 length)))
   (subs bits length)])

;; packet decoding

(defn decode-literal [bits]
  (loop [[value bits] [0 bits]]
    (let [[flag bits] (read-bits bits 1)
          [data bits] (read-bits bits 4)
          new-value [(+ (* value 16) data) bits]]
      (case flag
        0 new-value
        1 (recur new-value)))))

(declare decode-packet)

(defn decode-subs [bits]
  (let [[type bits] (read-bits bits 1)]
    (case type
     0 (let [[length bits] (read-bits bits 15)
             end-pos (- (count bits) length)]
         (loop [subs []
                bits bits]
           (if (= (count bits) end-pos)
             [subs bits]
             (let [[sub bits] (decode-packet bits)]
               (recur (conj subs sub) bits)))))
     1 (let [[length bits] (read-bits bits 11)]
         (loop [length length
                bits bits
                subs []]
          (if (zero? length)
            [subs bits]
            (let [[sub offset] (decode-packet bits)]
              (recur (dec length) offset (conj subs sub)))))))))

(defn decode-packet [bits]
  (let [[version bits] (read-bits bits 3)
        [tag bits] (read-bits bits 3)]
    (case tag
      4 (let [[literal bits] (decode-literal bits)]
          [{:version version, :tag tag, :value literal} bits])
      (let [[subs bits] (decode-subs bits)]
        [{:version version :tag tag :subs subs} bits]))))

(defn version-total [packet]
  (+ (:version packet)
     (reduce + (map version-total (:subs packet)))))

(defn part1 []
  (->> (parse-input input)
       decode-packet
       first
       version-total))

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
  (->> (parse-input input)
       decode-packet
       first
       eval-packet))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
(ns main
  (:require [clojure.java.io :as io]))

(defn -main [& _args]
  (->> (io/file "./src")
       file-seq
       (map #(re-find #"^day(\d+).clj$" (.getName %)))
       (filter some?)
       (map last)
       (map read-string)
       sort
       (map (fn [day]
              (require (symbol (str "day" day)))
              (doall
                (for [part [1 2]]
                  (-> (symbol (str "day" day "/part" part))
                      (doto (print ": "))
                      (#(time ((eval %)))))))))
       doall)
  "done")
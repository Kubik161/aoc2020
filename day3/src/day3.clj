(ns day3
  "AOC 2020 Day 3"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn count-trees [input, row, column, tree-count, row-step, column-step]
  (if (< row (alength input))
    (count-trees input (+ row-step row) (mod (+ column-step column) 31) (if (identical? (aget input row column) \#) (inc tree-count) tree-count) row-step column-step)
    tree-count))

(defn solve-day3a [input]
  (count-trees input 0 0 0 1 3))

(defn solve-day3b [input]
  (* (count-trees input 0 0 0 1 1)
     (count-trees input 0 0 0 1 3)
     (count-trees input 0 0 0 1 5)
     (count-trees input 0 0 0 1 7)
     (count-trees input 0 0 0 2 1)
     ))

;; rows 323, line length 31
(defn string-vector-to-2d-array [vector]
  (to-array-2d (map seq vector)))

(defn -main
  "AOC Day 3"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         string-vector-to-2d-array
         ;solve-day3a
         solve-day3b
         println)))
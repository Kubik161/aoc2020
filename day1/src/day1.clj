(ns day1
  "AOC 2020 Day 1"
  (:require
   [clojure.string :as string])
  (:gen-class))


(defn solve-day1a [s]
  (let [x (first s)
        rest (drop 1 s)
        y (some #(when (= 2020 (+ % x)) %) rest)]
    (if y
      (* x y)
      (solve-day1a rest))))

(defn solve-day1b-step-2 [rest-x x]
  (if (= 0 (count rest-x))
    nil
    (let [y (first rest-x)
          rest-x-y (drop 1 rest-x)
          z (some #(when (= 2020 (+ % x y)) %) rest-x-y)]
      (if z
        (* x y z)
        (solve-day1b-step-2 rest-x-y x)))))

(defn solve-day1b [s]
  (let [x (first s)
        rest-x (drop 1 s)]
    (if-let [result (solve-day1b-step-2 rest-x x)]
      result
      (solve-day1b rest-x))))

(defn string-vector-to-int [vector]
  (map #(Integer/parseInt %) vector))

(defn -main
  "AOC Day 1"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         string-vector-to-int
         ;solve-day1a
         solve-day1b
         println)))
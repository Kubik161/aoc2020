(ns day6
  "AOC 2020 Day 6"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn group-answer [input]
  (count (distinct input)))

(defn solve-day6a [input]
  (reduce + (map group-answer input)))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn group-answer-b [input]
  (let [group-size (count input)]
    (count (filter #(in? % group-size) (frequencies (string/join input))))))

(defn solve-day6b [input]
  (reduce + (map group-answer-b input)))

(defn split-by-empty-line [input]
  (string/split input #"\r?\n\r?\n" -1))

(defn normalize-a [vector]
  (map #(string/trim (string/join (string/split-lines %))) vector))

(defn normalize-b [vector]
  (map #(string/split-lines %) vector))

(defn -main
  "AOC Day 6"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         split-by-empty-line
         ;normalize-a
         ;solve-day6a
         normalize-b
         solve-day6b
         println)))
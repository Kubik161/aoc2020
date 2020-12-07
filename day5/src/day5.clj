(ns day5
  "AOC 2020 Day 5"
  (:require
   [clojure.string :as string])
  (:gen-class))

; FBFBBFF = 0101100
(defn item-to-seat-id [item]
  (+ (* (:row item) 8) (:column item)))

(defn binary-string-to-number [input]
  (let [seat (subs input 0 7)
        column (subs input 7)]
  (hash-map :row (Integer/parseInt (string/replace (string/replace seat #"B" "1") #"F" "0") 2)
            :column (Integer/parseInt (string/replace (string/replace column #"R" "1") #"L" "0") 2))))

(defn solve-day5a [input]
  (reduce max (map #(item-to-seat-id (binary-string-to-number %)) input)))

(defn check-if-next-missing [current seats]
  (let [next (first seats)]
    (if (= (inc current) next)
      (check-if-next-missing next (drop 1 seats)) 
      (inc current))))

(defn solve-day5b [input]
  (let [seats (sort (map #(item-to-seat-id (binary-string-to-number %)) input))]
  (check-if-next-missing (first seats) (drop 1 seats))))

(defn -main
  "AOC Day 5"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         ;solve-day5a
         solve-day5b
         println)))
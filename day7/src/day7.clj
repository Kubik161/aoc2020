(ns day7
  "AOC 2020 Day 7"
  (:require
   [clojure.string :as string]
   [clojure.set])
  (:gen-class))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn remove-processed [input output]
  (filter #(not (in? output %)) input))

(defn map-output [output]
  (map #(:bag %) output))

(defn contains-gold-bag [current-contain output]
  (if (empty? current-contain)
      false
  (if (some #(= % (first current-contain)) output) 
    true
      (contains-gold-bag (drop 1 current-contain) output))))

(defn find-gold-bags [input output]
  (let [next-input (remove-processed input output)
        mapped-output (map-output output)
        new-shiny (filter #(contains-gold-bag (:contain %) mapped-output) next-input)
        next-output (concat output new-shiny)]
    (if (= (count output) (count next-output))
      next-output
      (find-gold-bags next-input next-output)))) 

(defn prepare-initial-bags [input]
  (filter #(in? (:contain %) "shiny gold") input))

(defn solve-day7a [input]
  (find-gold-bags input (prepare-initial-bags input)))

(defn normalize-item [item]
  (let [matcher (re-matcher #"(.*) bags contain (.*)\." item)
        find (re-find matcher)
        match (re-groups matcher)
        splitted (string/split (nth match 2) #",")]
    (hash-map :bag (second match) :contain (map #(string/trim (subs % 2 (- (count %) 4))) splitted))))

(defn normalize-input [input]
  (map normalize-item input))

(defn remove-no-bags [input]
  (filter #(not (or (re-find #"no other bags" %)
                    (re-find #"shiny gold bags contain" %))) input)) 

(defn -main
  "AOC Day 7"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         remove-no-bags
         normalize-input
         solve-day7a
         ;normalize-b
         ;solve-day6b
         count
         println)))
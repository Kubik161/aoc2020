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

(defn find-bag [name data]
  (some #(when (= name (:bag %)) %) data))

(defn value-of [bag-name data]
  (let [bag (find-bag bag-name data)]
    (if (:value bag)
      (:value bag)
      (inc (reduce + (map #(* (:count %) (value-of (:bag %) data)) (:contain bag)))))))

(defn solve-day7b [input]
  (dec (value-of "shiny gold" input)))

(defn normalize-item-b [item]
  (let [matcher (re-matcher #"(.*) bags contain (.*)\." item)
        find (re-find matcher)
        match (re-groups matcher)
        splitted (string/split (nth match 2) #",")]
    (hash-map :bag (second match) :contain (map #(hash-map :bag (string/trim (subs % 2 (- (count %) 4))) :count (Integer/parseInt (string/trim (subs % 0 2)))) splitted))))

(defn prepare-default-values []
  (vector (hash-map :bag "posh maroon" :value 1)
          (hash-map :bag "posh yellow" :value 1)
          (hash-map :bag "wavy coral" :value 1)
          (hash-map :bag "wavy gray" :value 1)
          (hash-map :bag "drab brown" :value 1)
          (hash-map :bag "pale lime" :value 1)
          (hash-map :bag "dim plum" :value 1)
          (hash-map :bag "vibrant tan" :value 1)
          (hash-map :bag "dim crimson" :value 1)
          (hash-map :bag "vibrant orange" :value 1)
          (hash-map :bag "shiny gold" :contain (list (hash-map :bag "wavy green" :count 4)
                                                     (hash-map :bag "mirrored teal" :count 2)
                                                     (hash-map :bag "dark tomato" :count 4)
                                                     (hash-map :bag "faded beige" :count 2)))))

(defn normalize-input-b [input]
  (concat (prepare-default-values) (map normalize-item-b input)))

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
         ;normalize-input
         ;solve-day7a
         normalize-input-b
         solve-day7b
         println)))
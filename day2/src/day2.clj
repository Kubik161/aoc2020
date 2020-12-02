(ns day2
  "AOC 2020 Day 2"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn solve-item [item]
  (let [occurence (find (frequencies (:password item)) (:char item))]
    (<= (:min item) (if (some? occurence) (second occurence) 0) (:max item))))

(defn solve-item-b [item]
  (let [first-occurence (= (subs (:password item) (dec (:min item)) (:min item)) (str (:char item)))
        second-occurence (= (subs (:password item) (dec (:max item)) (:max item)) (str (:char item)))]
    (or (and first-occurence
             (not second-occurence))
        (and (not first-occurence)
             second-occurence))))

(defn solve-day2a [input]
  (count (filter solve-item input)))

(defn solve-day2b [input]
  (count (filter solve-item-b input)))

(defn parse-line [line]
  (let [split (first (string/split line #": "))
        password (second (string/split line #": "))
        char (last split)
        minmax (first (string/split split #" "))
        min (Integer/parseInt (first (string/split minmax #"-")))
        max (Integer/parseInt (second (string/split minmax #"-")))]
    (hash-map :char char, :min min :max max :password password)))

;; :char x, :min 2 :max 5 :password xaxaxa
(defn string-vector-to-data [vector]
  (map parse-line vector))

(defn -main
  "AOC Day 2"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         string-vector-to-data
         ;;solve-day2a
         solve-day2b
         println)))
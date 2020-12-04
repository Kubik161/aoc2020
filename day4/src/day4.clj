(ns day4
  "AOC 2020 Day 4"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn valid-byr? [value]
  (and (= (count value) 4)
       (<= 1920 (Integer/parseInt value) 2002)))

(defn valid-iyr? [value]
  (and (= (count value) 4)
       (<= 2010 (Integer/parseInt value) 2020)))

(defn valid-eyr? [value]
  (and (= (count value) 4)
       (<= 2020 (Integer/parseInt value) 2030)))

(defn valid-hgt? [value]
  (let [units (subs value (- (count value) 2) (count value))
        number (Integer/parseInt (subs value 0 (- (count value) 2)))]
    (or (and (= units "cm")
             (<= 150 number 193))
        (and (= units "in")
             (<= 59 number 76)))))

(defn valid-hcl? [value]
  (let [hashtag (subs value 0 1)
        color (subs value 1 (count value))]
    (and (= hashtag (str \#))
         (re-find #"[a-f0-9]{6}" color ))))

(defn valid-ecl? [value]
    (or (= value "amb")
        (= value "blu")
        (= value "brn")
        (= value "gry")
        (= value "grn")
        (= value "hzl")
        (= value "oth")))

(defn valid-pid? [value]
  (= (count value) 9))

(defn valid-passport-b? [map]
  (and (and (some? (:byr map))
            (valid-byr? (:byr map)))
       (and (some? (:iyr map))
            (valid-iyr? (:iyr map)))
       (and (some? (:eyr map))
            (valid-eyr? (:eyr map)))
       (and (some? (:hgt map))
            (valid-hgt? (:hgt map)))
       (and (some? (:hcl map))
            (valid-hcl? (:hcl map)))
       (and (some? (:ecl map))
            (valid-ecl? (:ecl map)))
       (and (some? (:pid map))
            (valid-pid? (:pid map)))))

(defn valid-passport? [map]
  (and (some? (:byr map))
       (some? (:iyr map))
       (some? (:eyr map))
       (some? (:hgt map))
       (some? (:hcl map))
       (some? (:ecl map))
       (some? (:pid map))))

(defn solve-day4a [input]
  (count (filter valid-passport? input)))

(defn solve-day4b [input]
  (count (filter valid-passport-b? input)))

(defn split-by-whitespace [input]
  (string/split input #"\s+"))

(defn split-by-semicolon [input]
  (string/split input #":"))

(defn normalize-item [line]
  (let [line-split (split-by-whitespace line)
        keys (map #(keyword (first (split-by-semicolon %))) line-split)
        vals (map #(second (split-by-semicolon %)) line-split)]
    (zipmap keys vals)))

(defn normalize [vector]
  (map normalize-item vector))

(defn split-by-empty-line [input]
  (string/split input #"\r?\n\r?\n" -1))

(defn -main
  "AOC Day 4"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         split-by-empty-line
         normalize
         ;solve-day4a
         solve-day4b
         println)))
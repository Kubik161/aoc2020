(ns day9
  "AOC 2020 Day 9"
  (:require
   [clojure.string :as string]
   [clojure.set])
  (:gen-class))

(defn is-number-valid? [number preamble]
  (let [first-preamb (first (subvec preamble 0 1))
        rest-preamb (subvec preamble 1 (count preamble))]
    (if (some #(= number (+ % first-preamb)) rest-preamb)
      true
      (if (= 1 (count rest-preamb))
        false
        (is-number-valid? number rest-preamb)))))

(defn validate-next [preamble input]
  (let [next-number (first input)]
    (if (is-number-valid? next-number preamble)
      (validate-next (conj (subvec preamble 1 (count preamble)) next-number) (subvec input 1 (count input)))
      next-number)))

(defn solve-day9a [input]
  (println (type input))
  (let [preamble (subvec input 0 25)]
    (validate-next preamble (subvec input 25 (count input)))))

(defn find-contiguous [input target]
  (reduce (fn [acc next]
            (if (<= (+ (:total acc) next) target)
              (assoc acc :total (+ (:total acc) next) :max (max (:max acc) next) :min (min (:min acc) next))
              (reduced acc))) 
          (hash-map :total 0 :min (first input) :max (first input))
          input))

;177777905
(defn solve-day9b [input]
  (let [result (find-contiguous input 177777905)]
    (if (= 177777905 (:total result))
      (+ (:min result) (:max result))
      (solve-day9b (subvec input 1 (count input))))))

(defn normalize-input [input]
  (vec (map #(Long/parseLong %) input)))

(defn -main
  "AOC Day 9"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines      
         normalize-input
         ;solve-day9a
         ;;177777905
         solve-day9b
         println)))
(ns day10
  "AOC 2020 Day 10"
  (:require
   [clojure.string :as string]
   [clojure.set])
  (:gen-class))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn count-differences [input]
  (reduce (fn [acc next]
            (cond
              (= 1 (- next (:previous acc))) (assoc acc :previous next :diff-1 (inc (:diff-1 acc)))
              (= 2 (- next (:previous acc))) (assoc acc :previous next :diff-2 (inc (:diff-2 acc)))
              (= 3 (- next (:previous acc))) (assoc acc :previous next :diff-3 (inc (:diff-3 acc)))
              :else (println "broken"))
          )
          (hash-map :previous 0 :diff-1 0 :diff-2 0 :diff-3 1)
          input))

(defn solve-day10a [input]
  (let [result (count-differences input)]
    (* (:diff-1 result) (:diff-3 result))))

(defn count-differences-b [input]
  (reduce (fn [acc next]
            (if (and (= 1 (- next (:previous acc)))
                     (= 1 (:previous-diff acc)))
              (assoc acc :previous next :previous-diff (- next (:previous acc)) :count-1 (inc (:count-1 acc)))
              (let [group2 (if (= 1 (:count-1 acc)) (inc (:group-2 acc)) (:group-2 acc))
                    group3 (if (= 2 (:count-1 acc)) (inc (:group-3 acc)) (:group-3 acc))
                    group4 (if (= 3 (:count-1 acc)) (inc (:group-4 acc)) (:group-4 acc))]
                  (assoc acc :previous next :previous-diff (- next (:previous acc)) :group-2 group2 :group-3 group3 :group-4 group4 :count-1 0))))
          (hash-map :previous 0 :previous-diff 0 :group-2 0 :group-3 0 :group-4 0 :count-1 0)
          (conj input (+ (last input) 3))))

(defn solve-day10b [input]
  (let [result (count-differences-b input)]
    (* (exp 2 (:group-2 result))
       (exp 4 (:group-3 result))
       (exp 7 (:group-4 result))
       )))

(defn normalize-input [input]
  (vec (sort (map #(Long/parseLong %) input))))

(defn -main
  "AOC Day 10"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines      
         normalize-input
         ;solve-day10a
         solve-day10b
         println)))
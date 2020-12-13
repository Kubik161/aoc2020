(ns day13
  "AOC 2020 Day 13"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn abs [n] (max n (- n)))

(defn solve-day13a [input]
  (let [target (:target input)
        result (reduce (fn [acc item]
                         (println item)
                         (println acc)
                         (let [wait-time (- item (mod target item))]
                           (if (< wait-time (:wait acc))
                             (assoc acc :wait wait-time :bus item)
                             acc))) (hash-map :wait 1000000000 :bus 0) (:buses input))]
    (* (:bus result) (:wait result))
    ))

(defn matched? [item]
  (or
   ;;if wait time is equal to offset
   (=
    (- (:bus item)
       (mod (- (:target item)
               (:target-offset item))
            (:bus item)))
    (:offset item))
      ;;if wait time 0 and offset 0 
   (and (= (mod (- (:target item)
                   (:target-offset item))
                (:bus item))
           0)
        (= (:offset item)
           0))))

;{:offset 4, :bus 59 :target 156114 :target-offset 4}
(defn generate-next-input [input highest]
  (map (fn [item] (assoc item :target (+ (:target item) highest))) input))

(defn solve-b [in highest]
  (loop [input in]
    (println (:target (first input)))
    (if
     (= (count (filter matched? input)) (count input))
      (- (:target (first input)) (:target-offset (first input)))
      (recur (generate-next-input input highest)))))

(defn solve-day13b [input]
  (let [highest (reduce (fn [acc next] (if (< (:bus acc) (:bus next))
                                         next
                                         acc)) input)
        mapped (map #(assoc % :target-offset (:offset highest) :target (+ (:bus highest) 100000000000174)) input)]
        ;mapped (map #(assoc % :target-offset (:offset highest) :target (:bus highest)) input)]
    (solve-b mapped (:bus highest))
    ))

(defn parse-buses [input]
  (let [values (string/split input #",")]
    (filter #(not (= "x" %)) values)))

(defn normalize [input]
  (hash-map :target (Integer/parseInt (first input)) :buses (map #(Integer/parseInt %) (parse-buses (second input)))))

(defn normalize-b [input]
  (let [mapped (map-indexed #(hash-map :bus %2 :offset %) (string/split (second input) #","))
        filtered (filter #(not (= "x" (:bus %))) mapped)
        parsed (map #(assoc % :bus (Integer/parseInt (:bus %))) filtered)]
    parsed))

(defn -main
  "AOC Day 13"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         ;normalize 
         ;solve-day13a
         normalize-b
         solve-day13b
         println)))
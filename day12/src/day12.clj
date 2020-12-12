(ns day12
  "AOC 2020 Day 12"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn abs [n] (max n (- n)))

;;east, south, west, north
(defn rotate-right [steps value]
  (if (= steps 0)
    value
    (rotate-right (- steps 90) (cond
                                 (= value "east") "south"
                                 (= value "south") "west"
                                 (= value "west") "north"
                                 (= value "north") "east"))))

;east, north, west, south
(defn rotate-left [steps value]
  (if (= steps 0)
    value
    (rotate-left (- steps 90) (cond
                                (= value "east") "north"
                                (= value "north") "west"
                                (= value "west") "south"
                                (= value "south") "east"))))

(defn process-next [positition item]
  (let [first-char (get item 0)
        value (Integer/parseInt (subs item 1 (count item)))]
    (cond
      (= first-char \N) (assoc positition :north (+ (:north positition) value))
      (= first-char \S) (assoc positition :south (+ (:south positition) value))
      (= first-char \E) (assoc positition :east (+ (:east positition) value))
      (= first-char \W) (assoc positition :west (+ (:west positition) value))
      (= first-char \F) (assoc positition (keyword (:forward positition)) (+ ((keyword (:forward positition)) positition) value))
      (= first-char \R) (assoc positition :forward (rotate-right value (:forward positition)))
      (= first-char \L) (assoc positition :forward (rotate-left value (:forward positition))))))

(defn solve [input positition]
  (if (empty? input)
    positition
    (let [next (first input)
          new-input (rest input)]
      (solve new-input (process-next positition next)))))

(defn solve-day12a [input]
  (let [positition (hash-map :north 0 :south 0 :west 0 :east 0 :forward "east")
        solved (solve input positition)
        x (abs (- (:east solved) (:west solved)))
        y (abs (- (:north solved) (:south solved)))]
    (+ x y)))

(defn rotate-right-b [steps position]
  (if (= steps 0)
    position
    (rotate-right-b (- steps 90) (assoc position :w-x (:w-y position) :w-y (* (:w-x position) -1)))))

(defn rotate-left-b [steps position]
  (if (= steps 0)
    position
    (rotate-left-b (- steps 90) (assoc position :w-x (*  (:w-y position) -1) :w-y (:w-x position)))))

(defn process-next-b [position item]
  (let [first-char (get item 0)
        value (Integer/parseInt (subs item 1 (count item)))]
    (cond
      (= first-char \N) (assoc position :w-y (+ (:w-y position) value))
      (= first-char \S) (assoc position :w-y (- (:w-y position) value))
      (= first-char \E) (assoc position :w-x (+ (:w-x position) value))
      (= first-char \W) (assoc position :w-x (- (:w-x position) value))
      (= first-char \F) (assoc position :y (+ (:y position) (* value (:w-y position))) :x (+ (:x position) (* value (:w-x position))))
      (= first-char \R) (rotate-right-b value position)
      (= first-char \L) (rotate-left-b value position))))

(defn solve-b [input positition]
  (if (empty? input)
    positition
    (let [next (first input)
          new-input (rest input)]
      (solve-b new-input (process-next-b positition next)))))

(defn solve-day12b [input]
  (let [positition (hash-map :y 0 :x 0
                             :w-y 1 :w-x 10)
        solved (solve-b input positition)]
    (+ (abs (:x solved)) (abs (:y solved)))))

(defn -main
  "AOC Day 12"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         ;solve-day12a
         solve-day12b
         println)))
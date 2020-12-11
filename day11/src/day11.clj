(ns day11
  "AOC 2020 Day 11"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn get-seat-state [state index]
  (let [seat (nth state index)]
    (if (:occupieable seat)
      (:occupied seat)
      -2)))

(defn get-top-left [state index max-x]
  (if (or (neg? (- index (+ max-x 1))) (neg? (- (mod index max-x) 1))) -1 (get-seat-state state (- index (+ max-x 1)))))

(defn get-top-middle [state index max-x]
  (if (neg? (- index max-x)) -1 (get-seat-state state (- index max-x))))

(defn get-top-right [state index max-x]
  (if (or (neg? (- index (- max-x 1))) (<= max-x (+ (mod index max-x) 1))) -1 (get-seat-state state (- index (- max-x 1)))))

(defn get-left [state index max-x]
  (if (neg? (- (mod index max-x) 1)) -1 (get-seat-state state (- index 1))))

(defn get-right [state index max-x]
  (if (or (<= (count state) (+ index 1)) (<= max-x (+ (mod index max-x) 1))) -1 (get-seat-state state (+ index 1))))

(defn get-bottom-left [state index max-x]
  (if (or (<= (count state) (+ index (- max-x 1))) (neg? (- (mod index max-x) 1))) -1 (get-seat-state state (+ index (- max-x 1)))))

(defn get-bottom-middle [state index max-x]
  (if (<= (count state) (+ index max-x)) -1 (get-seat-state state (+ index max-x))))

(defn get-bottom-right [state index max-x]
  (if (or (<= (count state) (+ index (+ max-x 1))) (<= max-x (+ (mod index max-x) 1))) -1 (get-seat-state state (+ index (+ max-x 1)))))

(defn get-top-left-b [state index max-x]
  (let [result (get-top-left state index max-x)]
    (if (= -2 result)
      (get-top-left-b state (- index (inc max-x)) max-x)
      result)))

(defn get-top-middle-b [state index max-x]
  (let [result (get-top-middle state index max-x)]
    (if (= -2 result)
      (get-top-middle-b state (- index max-x) max-x)
      result)))

(defn get-top-right-b [state index max-x]
  (let [result (get-top-right state index max-x)]
    (if (= -2 result)
      (get-top-right-b state (- index (dec max-x)) max-x)
      result)))

(defn get-right-b [state index max-x]
  (let [result (get-right state index max-x)]
    (if (= -2 result)
      (get-right-b state (inc index) max-x)
      result)))

(defn get-left-b [state index max-x]
  (let [result (get-left state index max-x)]
    (if (= -2 result)
      (get-left-b state (dec index) max-x)
      result)))

(defn get-bottom-left-b [state index max-x]
  (let [result (get-bottom-left state index max-x)]
    (if (= -2 result)
      (get-bottom-left-b state (+ index (dec max-x)) max-x)
      result)))

(defn get-bottom-middle-b [state index max-x]
  (let [result (get-bottom-middle state index max-x)]
    (if (= -2 result)
      (get-bottom-middle-b state (+ index max-x) max-x)
      result)))

(defn get-bottom-right-b [state index max-x]
  (let [result (get-bottom-right state index max-x)]
    (if (= -2 result)
      (get-bottom-right-b state (+ index (inc max-x)) max-x)
      result)))

(defn count-adjacent-seats [state index]
  (let [max-x 10
        tl (get-top-left state index max-x)
        tm (get-top-middle state index max-x)
        tr (get-top-right state index max-x)
        l (get-left state index max-x)
        r (get-right state index max-x)
        bl (get-bottom-left state index max-x)
        bm (get-bottom-middle state index max-x)
        br (get-bottom-right state index max-x)]
    (+ (if (neg? tl) 0 tl)
       (if (neg? tm) 0 tm)
       (if (neg? tr) 0 tr)
       (if (neg? l) 0 l)
       (if (neg? r) 0 r)
       (if (neg? bl) 0 bl)
       (if (neg? bm) 0 bm)
       (if (neg? br) 0 br))))

(defn changed? [input]
  (some #(:changed %) input))

(defn to-string [result]
  (reduce (fn [acc next] (str acc (cond
                                    (not (:occupieable next)) "."
                                    (= 1 (:occupied next)) "#"
                                    (= 0 (:occupied next)) "L"
                                    :else "X")))
          ""
          result))

(defn re-solve [input loop-count occupied-limit]
  (let [result (map-indexed (fn [index item] (let [adjacent-occupied (count-adjacent-seats input index)
                                                   adjacent-occupied-fixed (if (= -1 adjacent-occupied) 0 adjacent-occupied)
                                                   new-occupied (if (:occupieable item) (cond
                                                                                          (and (= 0 (:occupied item)) (zero? adjacent-occupied-fixed)) 1
                                                                                          (and (= 1 (:occupied item)) (<= occupied-limit adjacent-occupied-fixed)) 0
                                                                                          :else (:occupied item)) 0)]
                                               (assoc item
                                                      :occupied new-occupied
                                                      :changed (not (= new-occupied (:occupied item)))))) input)]
    (if (changed? result)
      (re-solve result (inc loop-count) occupied-limit)
      (count (filter #(= 1 (:occupied %)) result)))))

(defn solve-day11a [input]
  (re-solve input 0 4))

(defn count-adjacent-seats-b [state index]
  (let [max-x 10
        tl (get-top-left-b state index max-x)
        tm (get-top-middle-b state index max-x)
        tr (get-top-right-b state index max-x)
        l (get-left-b state index max-x)
        r (get-right-b state index max-x)
        bl (get-bottom-left-b state index max-x)
        bm (get-bottom-middle-b state index max-x)
        br (get-bottom-right-b state index max-x)]
    (+ (if (neg? tl) 0 tl)
       (if (neg? tm) 0 tm)
       (if (neg? tr) 0 tr)
       (if (neg? l) 0 l)
       (if (neg? r) 0 r)
       (if (neg? bl) 0 bl)
       (if (neg? bm) 0 bm)
       (if (neg? br) 0 br))))

(defn re-solve-b [input loop-count occupied-limit]
  (let [result (map-indexed (fn [index item] (let [adjacent-occupied (count-adjacent-seats-b input index)
                                                   new-occupied (if (:occupieable item) (cond
                                                                                          (and (= 0 (:occupied item)) (zero? adjacent-occupied)) 1
                                                                                          (and (= 1 (:occupied item)) (<= occupied-limit adjacent-occupied)) 0
                                                                                          :else (:occupied item)) 0)]
                                               (assoc item
                                                      :occupied new-occupied
                                                      :changed (not (= new-occupied (:occupied item)))))) input)]
    (if (changed? result)
      (re-solve-b result (inc loop-count) occupied-limit)
      (count (filter #(= 1 (:occupied %)) result)))))

(defn solve-day11b [input]
  (re-solve-b input 0 5))

(defn prepare-field [item]
  (map #(hash-map :occupied 0 :occupieable (if (= (str %) ".") false true) :changed false) item))

(defn string-vector-to-2d-array [vector]
  (flatten (map #(prepare-field (seq %)) vector)))

(defn -main
  "AOC Day 11"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines
         string-vector-to-2d-array
         ;solve-day11a
         solve-day11b
         println)))
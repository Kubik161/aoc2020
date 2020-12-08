(ns day8
  "AOC 2020 Day 8"
  (:require
   [clojure.string :as string]
   [clojure.set])
  (:gen-class))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn get-new-acc [old-acc instruction]
   (cond
     (= "acc" (:inst instruction)) (+ old-acc (:value instruction))
     :else old-acc))

(defn get-new-pos [old-pos instruction]
  (cond
    (= "jmp" (:inst instruction)) (+ old-pos (:value instruction))
    :else (inc old-pos)))

(defn process-instruction [instructions pos acc]
  (let [instruction (get instructions pos)]
    (cond
     (:used instruction) acc
     :else 
         (let [new-pos (get-new-pos pos instruction)
               new-acc (get-new-acc acc instruction)]
           (process-instruction (assoc-in instructions [pos :used] true) new-pos new-acc)))))

(defn solve-day8a [input]
  (process-instruction input 0 0))


(defn process-instruction-b [instructions pos acc]
  (let [instruction (get instructions pos)]
    (cond
      (:used instruction) (hash-map :solved false :value acc)
      (<= (count instructions) pos) (hash-map :solved true :value acc)
      :else
      (let [new-pos (get-new-pos pos instruction)
            new-acc (get-new-acc acc instruction)]
        (process-instruction-b (assoc-in instructions [pos :used] true) new-pos new-acc)))))

(defn modify-instructions [instructions changed-instruction]
  (let [instruction (get instructions changed-instruction)]
  (cond
    (= "jmp" (:inst instruction)) (assoc-in instructions [changed-instruction :inst] "nop")
    (= "nop" (:inst instruction)) (assoc-in instructions [changed-instruction :inst] "jmp")
    :else instructions
    )))

(defn solve-day8b
  ([input] (solve-day8b input 0))
  ([instructions changed-instruction]
   (if (= "acc" (:inst (get instructions changed-instruction)))
     (solve-day8b instructions (inc changed-instruction))
     (let [acc (process-instruction-b (modify-instructions instructions changed-instruction) 0 0)]
       (if (:solved acc)
         (:value acc)
         (solve-day8b instructions (inc changed-instruction)))))))

(defn normalize-item [index item]
  (let [inst (subs item 0 3)
        value (subs item 4 (count item))]
    (hash-map index (hash-map :inst inst :value (Integer/parseInt value)))))

(defn normalize-input [input]
  (reduce merge (map-indexed normalize-item input)))

(defn -main
  "AOC Day 8"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         slurp
         string/trim
         string/split-lines      
         normalize-input
         ;solve-day8a
         solve-day8b
         println)))
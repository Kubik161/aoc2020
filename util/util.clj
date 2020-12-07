(ns aoc2020util
  "AOC 2020 Util"
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn split-by-empty-line [input]
  (string/split input #"\r?\n\r?\n" -1))
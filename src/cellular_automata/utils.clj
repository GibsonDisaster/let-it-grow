(ns cellular-automata.utils
  (:require [clojure.string :as s]))

;; For generating a (square) board of size n with random values
(defn rand-board
  [n]
  (vec (for [x (range 0 n)
             y (range 0 n)]
         [[x y] (rand-nth [true false])])))

;; For loading the board from a file
(defn format-from-file
  [file-name]
  "Load file as a list of rows of numbers"
  (let [file-contents (slurp file-name)
        lines (s/split file-contents #"\n")
        cells (map #(s/split % #" ") lines)
        nums (for [l cells] (map #(Integer/parseInt %) l))]
    (vec (map vec nums))))

(defn convert-row
  [nums y s]
   (for [x (range (count nums))]
     [[x y] (= 1 (get nums x))]))

(defn from-file
  "Clean up file contents, then convert into key value lists"
  [file-name cell-size]
  (let [grid (format-from-file file-name)
        nums (for [y (range (count grid))]
               (convert-row (get grid y) y cell-size))
        flattened (apply concat nums)]
    flattened))

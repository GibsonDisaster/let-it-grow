(ns cellular-automata.core
  (:require [quil.core :as quil]
            [quil.middleware :as m]
            [cellular-automata.utils :as utils]))

(def cell-size 40)

(def test-file "resources/test.txt")

(def board (utils/rand-board (/ 800 cell-size)))
(def test-board (utils/from-file test-file cell-size))

(defn setup []
  (quil/frame-rate 30)
  (into {} test-board))

; pre-calculate all 8 directions a cell has a neighbor in
(def neighbor-coords
  (for [i [-1 0 1]
        j [-1 0 1]]
    [i j]))

(defn neighbors
  [[x y] cells]
  (let [neighbors (for [[neighbor-offset-x neighbor-offset-y] neighbor-coords
                        :let [neighbor-coord [(+ x neighbor-offset-x) (+ y neighbor-offset-y)]
                              alive? (get cells neighbor-coord)]
                        :when (and (some? alive?) (not= 0 neighbor-offset-x neighbor-offset-y) alive?)]
                    [[neighbor-offset-x neighbor-offset-y] alive?])]
    (count neighbors)))

(defn conway
  [cells]
  (for [[[x y] alive?] cells
        :let [neighbor-count (neighbors [x y] cells)]]
    (if alive?
      (cond
        (or (= neighbor-count 2) (= neighbor-count 3)) [[x y] true]
        (or (< neighbor-count 2) (> neighbor-count 3)) [[x y] false]
        :else [[x y] alive?])
      (if (= neighbor-count 3)
        [[x y] true]
        [[x y] false]))))

(defn handle-keys
  [state e]
  (if (= :space (:key e))
    (into {} (conway state))
    state))

(defn handle-mouse
  "Convert mouse coords to cell-coordinates then flip state if valid"
  [state e]
  (let [mx (int (/ (:x e) cell-size))
        my (int (/ (:y e) cell-size))
        val (get state [mx my])]
    (if (and (= :left (:button e)) (some? val))
      (update state [mx my] (constantly (not val)))
      state)))

(defn update-state
  [state]
  state)

(defn draw-cell
  [x y]
  (quil/rect (* x cell-size) (* y cell-size) cell-size cell-size))

(defn draw-state
  [state]
  (quil/background 0)
  (quil/stroke 255)
  (doseq [[[x y] a] state]
    (if a (quil/fill 220 220 220) (quil/fill 0 0 0))
    (draw-cell x y)))

(quil/defsketch cellular-automata
  :title "cellular automata"
  :size [805 805]
  :setup setup
  :update update-state
  :draw draw-state
  :mouse-clicked handle-mouse
  :key-typed handle-keys
  :features [:keep-on-top]
  :middleware [m/fun-mode])

; TODO
; save current cells as file
; load file with command arg? - gui? 

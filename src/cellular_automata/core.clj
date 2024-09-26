(ns cellular-automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [cellular-automata.utils :as utils]))

(def cell-size 40)

(def test-file "resources/test.txt")

(def board (utils/rand-board (/ 800 cell-size)))
(def test-board (utils/from-file test-file cell-size))

(defn setup []
  (q/frame-rate 30)
  (into {} test-board))

(defn neighbor-coords
  [x y]
  (for [i [-1 0 1]
        j [-1 0 1]]
    [i j]))

(defn neighbors
  [[x y] cells]
  (let [coords (neighbor-coords x y)
        neighbors (for [[cx cy] coords
                        :let [a (get cells [(+ x cx) (+ y cy)])]
                        :when (and (some? a) (not= 0 cx cy) a)] ; don't include itself or nil neighbors
                    [[cx cy] a])]
    (count neighbors)))


(defn conway
  [cells]
  (for [[[x y] a] cells
        :let [n (neighbors [x y] cells)]]
    (if a
      (cond
        (or (= n 2) (= n 3)) [[x y] true]
        (or (< n 2) (> n 3)) [[x y] false]
        :else [[x y] a])
      (if (= n 3)
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
  (q/rect (* x cell-size) (* y cell-size) cell-size cell-size))

(defn draw-state
  [state]
  (q/background 0)
  (q/stroke 255)
  (doseq [[[x y] a] state]
    (if a (q/fill 220 220 220) (q/fill 0 0 0))
    (draw-cell x y)))

(q/defsketch cellular-automata
  :title "cellular automat"
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

(ns advent.advent11
  [:require [clojure.pprint :refer [pprint]]])

(def test-data
  (to-array-2d
   [[5 4 8 3 1 4 3 2 2 3]
    [2 7 4 5 8 5 4 7 1 1]
    [5 2 6 4 5 5 6 1 7 3]
    [6 1 4 1 3 3 6 1 4 6]
    [6 3 5 7 3 8 5 4 7 8]
    [4 1 6 7 5 2 4 6 4 5]
    [2 1 7 6 8 4 1 7 2 1]
    [6 8 8 2 8 8 1 1 3 4]
    [4 8 4 6 8 4 8 5 5 4]
    [5 2 8 3 7 5 1 5 2 6]]))

(def input-data
  (to-array-2d
   [[6 6 1 7 1 1 3 5 8 4]
    [6 5 4 4 2 1 8 6 3 8]
    [5 4 5 7 3 3 1 4 8 8]
    [1 1 3 5 6 7 5 5 8 7]
    [1 2 2 1 3 5 3 2 1 6]
    [1 8 1 1 1 2 4 3 7 8]
    [1 3 8 7 8 6 4 3 6 8]
    [4 4 2 7 6 3 7 2 6 2]
    [6 7 7 8 6 4 5 4 8 6]
    [3 6 8 2 1 4 6 7 4 5]]))

(defn- tilemap-height
  [t]
  (alength t))

(defn- tilemap-width
  [t]
  (alength (aget t 0)))

(defn- one-step!
  [t]
  (let [q (atom [])]
    (doseq [x (range (tilemap-width t))]
      (doseq [y (range (tilemap-height t))]
        (let [v (inc (aget t x y))]
          (aset t x y v)
          (when (= v 10)
            (swap! q conj [x y])))))
    @q))

(defn- print-data
  [t]
  (doseq [x (range (tilemap-width t))]
    (doseq [y (range (tilemap-height t))]
      (print (aget t x y)))
    (println)))

(defn- neighborns
  [[x y]]
  [[(dec x) (dec y)]
   [x  (dec y)]
   [(inc x) (dec y)]

   [(dec x) y]
   [(inc x) y]

   [(dec x) (inc y)]
   [x  (inc y)]
   [(inc x) (inc y)]])

(defn- outside?
  [[x y] t]
  (or (< x 0)
      (< y 0)
      (>= x (tilemap-width t))
      (>= y (tilemap-height t))))

(defn- clear-cells!
  [t num]
  (doseq [x (range (tilemap-width t))]
    (doseq [y (range (tilemap-height t))]
      (when (> (aget t x y) 9)
        (aset t x y 0)
        (swap! num inc)))))

(defn- propagate!
  [q t]
  (let [q (atom q)]
    (while (not (empty? @q))
      (let [pos (peek @q)]
        (swap! q pop)
        (doseq [npos (neighborns pos)]
          (when (not (outside? npos t))
            (let [[x y] npos]
              (when (= (aset t x y (inc (aget t x y)))
                       10)
                (swap! q conj npos)))))))))

(defn- step!
  [t num]
  (let [q   (one-step! t)]
    (propagate! q t)
    (clear-cells! t num)))

(defn- all-equal?
  [t]
  (let [n (aget t 0 0)]
    (every? (fn [row] (every? #(= % n) row)) t)))

(defn resolve-part-1
  []
  (let [t   (aclone input-data)
        num (atom 0)]
    (doseq [n (range 100)]
      (step! t num))
    (println @num)))

(defn resolve-part-2
  []
  (let [t    (aclone input-data)
        num  (atom 0)
        step (atom 0)]
    (while (not (all-equal? t))
      (step! t num)
      (swap! step inc))
    (println @step)))

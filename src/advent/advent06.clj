(ns advent.advent06)

(def test-data
  [3 4 3 1 2])

(def input-data
  [1 1 1 2 1 5 1 1 2 1 4
   1 4 1 1 1 1 1 1 4 1 1
   1 1 4 1 1 5 1 3 1 2 1
   1 1 2 1 1 1 4 1 1 3 1
   5 1 1 1 1 3 5 5 2 1 1
   1 2 1 1 1 1 1 1 1 1 5
   4 1 1 1 1 1 3 1 1 2 4
   4 1 1 1 1 1 1 3 1 1 1
   1 5 1 3 1 5 1 2 1 1 5
   1 1 1 5 3 3 1 4 1 3 1
   3 1 1 1 1 3 1 4 1 1 1
   1 1 2 1 1 1 4 2 1 1 5
   1 1 1 2 1 1 1 1 1 1 1
   1 2 1 1 1 1 1 5 1 1 1
   1 3 1 1 1 1 1 3 4 1 2
   1 3 2 1 1 2 1 1 1 1 4
   1 1 1 1 4 1 1 1 1 1 2
   1 1 4 1 1 1 5 3 2 2 1
   1 3 1 5 1 5 1 1 1 1 1
   5 1 4 1 2 1 1 1 1 2 1
   3 1 1 1 1 1 1 2 1 1 1
   3 1 4 3 1 4 1 3 2 1 1
   1 1 1 3 1 1 1 1 1 1 1
   1 1 1 2 1 5 1 1 1 1 2
   1 1 1 3 5 1 1 1 1 5 1
   1 2 1 2 4 2 2 1 1 1 5
   2 1 1 5 1 1 1 1 5 1 1
   1 2 1])

(defn- group-fishes
  [fishes]
  (reduce (fn [state timer]
            (assoc state timer (inc (get state timer 0))))
          {}
          fishes))

(defn- dec-timers
  [state]
  (into {}
        (map (fn [index]
               [index (get state (inc index) 0)])
             (range 0 8))))

(defn- update-state
  [state]
  (let [new-state (dec-timers state)
        borns     (get state 0 0)]
    (assoc new-state
           8 borns
           6 (+ borns (get new-state 6 0)))))

(defn- count-fishes
  [state]
  (apply + (vals state)))

(defn- resolve-problem
  [max-generations data]
  (loop [generation 0
         state      (group-fishes data)]
    (println "gen" generation
             "population" (count-fishes state)
             "borns" (get state 8 0))
    (if (= generation max-generations)
      (count-fishes state)
      (recur (inc generation)
             (update-state state)))))

(defn resolve-test-part-1 []
  (resolve-problem 80 test-data))

(defn resolve-part-1 []
  (resolve-problem 80 input-data))

(defn resolve-part-2 []
  (resolve-problem 256 input-data))

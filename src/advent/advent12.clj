(ns advent.advent12
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def test-data
  ["start-A"
   "start-b"
   "A-c"
   "A-b"
   "b-d"
   "A-end"
   "b-end"])

(def input-data
  ["dr-of"
   "start-KT"
   "yj-sk"
   "start-gb"
   "of-start"
   "IJ-end"
   "VT-sk"
   "end-sk"
   "VT-km"
   "KT-end"
   "IJ-of"
   "dr-IJ"
   "yj-IJ"
   "KT-yj"
   "gb-VT"
   "dr-yj"
   "VT-of"
   "PZ-dr"
   "KT-of"
   "KT-gb"
   "of-gb"
   "dr-sk"
   "dr-VT"])

(defn- parse-data
  [data]
  (->> data
       (map #(str/split % #"-"))
       (map #(map keyword %))
       (reduce (fn [g [from to]]
                 (-> g
                     (update from conj to)
                     (update to conj from)))
               {})))

(defn- small-cove?
  [n]
  (= (str/lower-case (str n))
     (str n)))

(defn- only-one-small-cave
  [node path]
  (and (small-cove? node)
       (some #(= % node) path)))

(defn semi-flatten
  [x paths]
  (cond (not (sequential? x))
        paths

        (vector? x)
        (conj paths x)

        :else
        (reduce (fn [paths n]
                  (semi-flatten n paths))
                paths
                x)))

(defn- resolve-paths
  [g start-nodes contraintfn]
  (let [next-node (fn next-node [nodes path]
                    (map (fn [node]
                           (cond (contraintfn node path)
                                 path

                                 (= node :end)
                                 (conj path node)

                                 :else
                                 (next-node (g node) (conj path node))))
                         nodes))]
    (remove #(not= :end (last %))
            (semi-flatten (next-node start-nodes []) []))))

(defn resolve-part-1
  []
  (let [g     (parse-data input-data)
        paths (resolve-paths g [:start] only-one-small-cave)]
    (count paths)))

(defn- twice-small-cave
  [node path]
  (not (let [path (conj path node)]
         ;; start room only once
         (if (> (count (filter #(= :start %) path)) 1)
           false
           (let [path (filter small-cove? path)
                 path (remove #(= :start %) path)
                 fr   (frequencies path)]
             (and
              ;; small rooms only once except one that allows two
              (<= (count (remove (fn [[room counts]] (< counts 2))
                                 fr))
                  1)
              (not (some (fn [[room counts]] (> counts 2)) fr))))))))

(defn resolve-part-2
  []
  (let [g     (parse-data input-data)
      paths (resolve-paths g [:start] twice-small-cave)]
  (pprint (count paths))))

(ns day3.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))


; Part 1


(def lines
  (map #(str/split % #",")
       (str/split (slurp "resources/input.txt") #"\r\n")))

(defn create-point [points direction]
  (let [[last-x last-y] (last points)
        value (Integer/parseInt (str/join (drop 1 direction)))]
    (case (first direction)
      \U (conj points [last-x (+ last-y value)])
      \D (conj points [last-x (- last-y value)])
      \R (conj points [(+ last-x value) last-y])
      \L (conj points [(- last-x value) last-y]))))

(defn create-points [directions]
  (reduce create-point [[0 0]] directions))

(defn create-line [[[x1 y1] [x2 y2]]]
  (letfn [(getrange [a1 a2]
            (cond (= a1 a2) (repeat a1)
                  (<= a1 a2) (range a1 (inc a2))
                  (<= a2 a1) (range a1 (dec a2) -1)))]
    (let [xs (getrange x1 x2)
          ys (getrange y1 y2)]
      (map vector xs ys))))

(defn build-lines [points]
  (let [edges (partition 2 1 points)]
    (set (mapcat create-line edges))))

(defn manhatten-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn main [lines]
  (let [[graph1-points graph2-points] (map (comp build-lines create-points) lines)
        crosses (remove #{[0 0]} (set/intersection graph1-points graph2-points))
        distances (map manhatten-distance crosses)
        data (map (fn [cross distance] {:cross cross :distance distance}) crosses distances)]
    (apply min-key :distance data)))

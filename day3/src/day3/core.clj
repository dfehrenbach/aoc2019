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
      (remove #{[x1 y1]} (map vector xs ys))))) ;; Removes [0 0] so all distances will be 1 off

(defn build-lines [points]
  (let [edges (partition 2 1 points)]
    (mapcat create-line edges)))

(defn manhatten-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn part1-solution [lines]
  (let [[graph1-points graph2-points] (map (comp set build-lines create-points) lines)
        crosses (remove #{[0 0]} (set/intersection graph1-points graph2-points))
        distances (map manhatten-distance crosses)
        data (map (fn [cross distance] {:cross cross :distance distance}) crosses distances)]
    (apply min-key :distance data)))


; Part 2


(defn travelled-distance [crosses lines]
  (let [[graph1-path graph2-path] (map (comp build-lines create-points) lines)]
    (map (fn [cross] {:l1 (inc (.indexOf graph1-path cross)) ;; Count the removed [0 0] with inc
                      :l2 (inc (.indexOf graph2-path cross)) ;; COunt the removed [0 0] with inc
                      :cross cross})
         crosses)))

(defn part2-solution [lines]
  (let [[graph1-points graph2-points] (map (comp build-lines create-points) lines)
        crosses (remove #{[0 0]} (set/intersection (set graph1-points) (set graph2-points)))
        distances (travelled-distance crosses lines)
        {wire1-distance :l1 wire2-distance :l2} (apply min-key #(+ (:l1 %) (:l2 %)) distances)]
    (+ wire1-distance wire2-distance)))

(comment
  (part2-solution lines))
  ;; => 9240

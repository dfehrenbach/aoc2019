(ns day8.core
  (:require [clojure.string :as str]))

(def width 25)
(def height 6)

(defn format-input [rawinput]
  (->> rawinput
       str/split-lines
       (mapcat #(str/split % #""))
       (map #(Integer/parseInt %))))

(defn break-into-layers [input]
  (partition-all (* width height) input))

(defn build-layer-data [layer]
  (->> layer
       (group-by identity)
       (map (fn [[key nums]] {(keyword (str key)) (count nums)}))
       (apply merge)))

(defn multiply-ones-twos [{ones :1
                           twos :2}]
  (* ones twos))

(def part1
  (->> (slurp "resources/day8/input.txt")
       format-input
       break-into-layers
       (map build-layer-data)
       (apply min-key :0)
       multiply-ones-twos))

(comment part1)
  ;; => 2806 


(defn find-top-pixel [layered-pixels]
  (->> layered-pixels
       (drop-while #(= % 2))
       first))

(defn break-into-lines [input]
  (partition-all width input))

(def part2
  (->> (slurp "resources/day8/input.txt")
       format-input
       break-into-layers
       (apply map vector)
       (map find-top-pixel)
       break-into-lines
       (map println)))

;;(1 1 1 1 0 1 1 1 0 0 0 0 1 1 0 0 1 1 0 0 1 1 1 0 0)
;;(0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0)
;;(0 0 1 0 0 1 1 1 0 0 0 0 0 1 0 1 0 0 1 0 1 1 1 0 0)
;;(0 1 0 0 0 1 0 0 1 0 0 0 0 1 0 1 1 1 1 0 1 0 0 1 0)
;;(1 0 0 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0)
;;(1 1 1 1 0 1 1 1 0 0 0 1 1 0 0 1 0 0 1 0 1 1 1 0 0)

; Z B J A B

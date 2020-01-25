(ns day16.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [clojure.math.numeric-tower :as math]
            [clojure.core.reducers :as reducers]))

(def puzzle-input (slurp "resources/day16/input.txt"))

(def base-pattern [0 1 0 -1])

(defn format-input [input]
  (->> (first (str/split-lines input))
       str/split-lines
       first
       (map str)
       (map #(Integer/parseInt %))))

(defn create-pattern [idx]
  (mapcat #(repeat idx %) base-pattern))

(defn get-output [list idx]
  (let [pattern (create-pattern (inc idx))
        sum     (reduce + (mapv #(* %1 %2)
                                list
                                (drop 1 (cycle pattern))))]
    (math/abs (rem sum 10))))

(defn run-phase [list]
  (map (partial apply get-output)
       (map-indexed (fn [idx _] [list idx]) list)))

(defn part1 [input num-phases]
  (->> input
       format-input
       (iterate run-phase)
       ((fn [in] (nth in num-phases)))))

(defn run-phase2 [list]
  (reductions (fn [a b] (rem (+ a b) 10)) list))

(defn get-offset [list]
  (->> list
       (take 7)
       (str/join)
       (Integer/parseInt)))

(defn part2 [input num-phases]
  (let [in     (format-input input)
        offset (get-offset in)
        whole  (apply concat (repeat 10000 in))]
    (->> whole
         (drop offset)
         reverse
         (iterate run-phase2)
         ((fn [in] (nth in num-phases)))
         reverse)))

(comment
  (take 8 (part1 puzzle-input 100))
  ;; => (7 4 3 6 9 0 3 3)

  (take 8 (part2 puzzle-input 100))
  ;; => (1 9 9 0 3 8 6 4)

  0)

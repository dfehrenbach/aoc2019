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

(comment
  (take 8 (part1 puzzle-input 100))
  ;; => (7 4 3 6 9 0 3 3)

  (->> puzzle-input
       format-input
       count
       (* 10000))
  ;; => 6500000 -- size of next input

  (->> puzzle-input
       format-input
       (take 7)
       (str/join)
       (Integer/parseInt))
  ;; => 5976733 -- offset
  0)


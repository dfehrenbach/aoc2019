(ns day17.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [day13.core2 :as day13]))

(def root-program (mapv
                    #(Long/parseLong %)
                    (str/split (slurp "resources/day17/input.txt") #",|\r\n|\n")))

(defn mapify-program [program]
  (reduce (fn [acc v] (assoc acc (count acc) (bigint v))) {} program))

(def mapped-root (mapify-program root-program))

(defn mapify-output [scaffold]
  (apply merge
         (mapcat (fn [row yidx]
                   (map (fn [char xidx]
                          {[xidx yidx] char})
                        row (range (count row))))
                 scaffold (range (count scaffold)))))


(defn intersection? [mapped-scaffold [point _]]
  (let [[x y]     point
        up        [x (+ y 1)]
        down      [x (- y 1)]
        left      [(+ x 1) y]
        right     [(+ x 1) y]
        scaffold? (fn [point] (= \# (get mapped-scaffold point)))]
    (pm/dump :otter)
    (every? scaffold? [point up down left right])))

(defn find-alignment-param [[[x y] _]] (* x y))

(defn part1 [input]
  (let [starting-program (day13/fresh-program input)
        outputs          (:outputs (day13/run-program starting-program))
        scaffold         (str/split-lines (apply str (map char outputs)))
        mapped-scaffold  (mapify-output scaffold)
        intersections    (filter (partial intersection? mapped-scaffold) mapped-scaffold)]
    (reduce + (map find-alignment-param intersections))))

(comment

  (part1 mapped-root)
  ;; => 2788

  (pm/logs)

  (pm/reset!)

  (def fresh-environment
    {:bot-position {:x 0 :y 0}
     :map          {[0 0] :nothing}
     :path         []})

  (defn go-direction [program direction]
    (day13/run-program
      (-> program
          (update :inputs conj direction)
          (assoc :waiting false)))))

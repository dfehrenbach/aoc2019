(ns day18.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [clojure.math.numeric-tower :as math]
            [clojure.core.reducers :as reducers]))

(def puzzle-input (slurp "resources/day18/input.txt"))

(def door-keys #{\a \b \c \d \e \f \g \h \i \j \k \l \m
                 \n \o \p \q \r \s \t \u \v \w \x \y \z})

(def doors #{\A \B \C \D \E \F \G \H \I \J \K \L \M
             \N \O \P \Q \R \S \T \U \V \W \X \Y \Z})

(defn create-board [string-arr]
  (apply merge (for [[row-idx row]  (map-indexed vector string-arr)
                     [col-idx item] (map-indexed vector row)]
                 {[col-idx row-idx] item})))

(defn find-start [board sym]
  "Returning location for a symbol"
  (ffirst (filter #(= (val %) sym) board)))

(defn wall? [board location]
  (or (= (get board location) \#)
      (nil? (get board location))))

(defn surrounding-area [[x y]]
  "Gives surrounding coordinates based on a coordinate passed in"
  {:up   [x (dec y)] :down  [x (inc y)]
   :left [(dec x) y] :right [(inc x) y]})

(defn possible-moves [board location]
  "Takes the board and location and gives back locations that are not walls
Note: Does not strip visited layers"
  (remove (partial wall? board)
          (vals (surrounding-area location))))

(defn bfs [board start]
  (loop [layers [#{start}]]
    (let [all-nearby (apply clojure.set/union
                            (map #(set (possible-moves board %))
                                 (last layers)))
          next-layer (set (remove (apply clojure.set/union (butlast layers))
                                  all-nearby))]
      (if (empty? next-layer) layers
          (recur (conj layers next-layer))))))

(defn part1 [input]
  (let [board        (-> input
                         clojure.string/split-lines
                         create-board)
        bfs-layers   (map-indexed (fn [depth layer]
                                    [(keep door-keys
                                           (map (partial get board)
                                                layer))
                                     depth])
                                  (bfs board (find-start board \@)))
        keys->depths (filter #(some door-keys (first %)) bfs-layers)]
    keys->depths))
                                        ; CRAAaaaaaaap
                                        ; Just realized that none of this will work how I want it tower
                                        ; The doors are going to go unnoticed and it's not like each key has an explicit shortest path
                                        ; The BFS just tells me what layer everything is on, not if it's blocking a particular thing
                                        ; The BFS is clean, but I may have to simply do a dfs searching for each letter and record the array of blocking doors
(comment

  (part1 puzzle-input)

  0)

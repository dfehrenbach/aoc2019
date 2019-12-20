(ns day12.core
  (:require [clojure.math.numeric-tower :as math]))

(def puzzle-input (slurp "resources/input.txt"))
(def test1-input (slurp "resources/test.txt"))
(def test2-input (slurp "resources/test2.txt"))
(def test3-input (slurp "resources/test3.txt"))

(defn match-line [line]
  (let [matcher (re-matcher #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>" line)
        [x y z] (rest (re-find matcher))]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)
     :z (Integer/parseInt z)}))

(defn format-input [input]
  (map match-line (clojure.string/split-lines input)))

(defn apply-gravity [moon1 moon2]
  (letfn [(adjustment-fn [current k]
            (cond
              (= (k moon1) (k moon2)) current
              (< (k moon2) (k moon1)) (dec current)
              (< (k moon1) (k moon2)) (inc current)))]
    (-> moon1
        (update :xv adjustment-fn :x)
        (update :yv adjustment-fn :y)
        (update :zv adjustment-fn :z))))

(defn apply-gravity-from-all-moons [moons]
  (map (fn [moon]
         (reduce (fn [acc-moon moon2] (apply-gravity acc-moon moon2))
                 moon
                 (remove #{moon} moons)))
       moons))

(defn update-positions [moons]
  (map (fn [moon]
         (-> moon
             (update :x + (:xv moon))
             (update :y + (:yv moon))
             (update :z + (:zv moon))))
       moons))

(defn energy [moon]
  (let [potential-energy (reduce + (map #(Math/abs %)
                                        (vals (select-keys moon [:x :y :z]))))
        kinetic-energy   (reduce + (map #(Math/abs %)
                                        (vals (select-keys moon [:xv :yv :zv]))))]
    (* kinetic-energy potential-energy)))

(defn update-moons [moons]
  (->> moons
       apply-gravity-from-all-moons
       update-positions
       vec))

(defn update-n-times [moons n]
  (loop [moons moons
         count 0]
    (if (= count n) moons
        (recur (update-moons moons)
               (inc count)))))

(defn part1 [input steps]
  (let [in                (format-input input)
        moons             (map #(merge % {:xv 0 :yv 0 :zv 0}) in)
        moons-after-steps (update-n-times moons steps)]
    (reduce + (map energy moons-after-steps))))

(defn search-repeat-axis [moons p v]
  (loop [moons moons
         count 0
         seen  #{}]
    (let [axis (mapv #(select-keys % [p v]) moons)]
      (if (contains? seen axis) count
          (recur (update-moons moons)
                 (inc count)
                 (conj seen axis))))))

(defn part2 [input]
  (let [in                   (format-input input)
        moons                (map #(merge % {:xv 0 :yv 0 :zv 0}) in)
        soonest-aligned-axis (pmap (fn [[p v]]
                                     (search-repeat-axis moons p v))
                                   [[:x :xv] [:y :yv] [:z :zv]])]
    (reduce math/lcm soonest-aligned-axis)))

(comment
  (part1 puzzle-input 1000)
  ;; => 7928

  ;; EXPECTED: 2772
  (part2 test1-input)
  ;; => 2772

  ;; EXPECTED: 4686774924
  (part2 test3-input)
  ;; => 4686774924

  (part2 puzzle-input)
  ;; => 518311327635164
  )

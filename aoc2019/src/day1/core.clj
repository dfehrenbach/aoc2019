(ns day1.core (:require [clojure.string :as str]))

(def module-masses
  (map #(Integer/parseInt %)
       (str/split (slurp "resources/day1/input.txt") #"\r\n")))


; Part 1


(defn calculate-fuel [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn solve-modules-weight [masses]
  (reduce + (map calculate-fuel masses)))

(def modules-weight (solve-modules-weight module-masses))
;; => 3422661



; PART 2


(defn calculate-recursive-fuel [initial-fuel]
  (loop [fuel             initial-fuel
         accumulated-fuel 0]
    (let [next-fuel (calculate-fuel fuel)]
      (if (neg? next-fuel) accumulated-fuel
          (recur next-fuel
                 (+ accumulated-fuel next-fuel))))))

(defn solve-recursive-modules-weight [masses]
  (reduce + (map calculate-recursive-fuel masses)))

(def total-weight (solve-recursive-modules-weight module-masses))
;; => 5131103

(ns day17.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [day13.core2 :as day13]))

(def root-program (mapv
                    #(Long/parseLong %)
                    (str/split (slurp "resources/day15/input.txt") #",|\r\n|\n")))

(defn mapify-program [program]
  (reduce (fn [acc v] (assoc acc (count acc) (bigint v))) {} program))

(def mapped-root (mapify-program root-program))

(comment
  (def fresh-environment
    {:bot-position {:x 0 :y 0}
     :map          {[0 0] :nothing}
     :path         []})

  (defn go-direction [program direction]
    (day13/run-program
      (-> program
          (update :inputs conj direction)
          (assoc :waiting false)))))

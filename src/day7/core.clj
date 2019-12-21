(ns day7.core
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [chan <!! >!! <! >!]]))

(def root-program (mapv
                   #(Integer/parseInt %)
                   (str/split (slurp "resources/day7/input.txt") #",|\r\n|\n")))

(defn day7-build-program [starting-program inchan outchan]
  (async/go-loop [program starting-program
                  program-index 0]
    (let
     [remaining-program (vec (drop program-index program))
      [op' p1' p2' p3]  remaining-program
      [op _ m1 m2 _m3]  (map #(Integer/parseInt (str %)) (reverse (str op')))
      p1                (if (pos-int? m1) p1' (get program p1'))
      p2                (if (pos-int? m2) p2' (get program p2'))]

      (case op
        1 (recur (assoc program p3 (+ p1 p2))
                 (+ 4 program-index))

        2 (recur (assoc program p3 (* p1 p2))
                 (+ 4 program-index))

        3 (let [input (async/<! inchan)]
            (recur (assoc program p1' input)
                   (+ 2 program-index)))

        4 (do
            (async/>! outchan p1)
            (recur program
                   (+ 2 program-index)))

        5 (recur program
                 (if (zero? p1) (+ 3 program-index) p2))

        6 (recur program
                 (if (zero? p1) p2 (+ 3 program-index)))

        7 (recur (assoc program p3 (if (< p1 p2) 1 0))
                 (+ 4 program-index))

        8 (recur (assoc program p3 (if (= p1 p2) 1 0))
                 (+ 4 program-index))

        9 :exit))))

(defn main-orchestrator [program input]
  (let [a-chan                (chan (async/sliding-buffer 2))
        b-chan                (chan (async/sliding-buffer 2))
        c-chan                (chan (async/sliding-buffer 2))
        d-chan                (chan (async/sliding-buffer 2))
        e-chan                (chan (async/sliding-buffer 2))
        [in1 in2 in3 in4 in5] input]

    (async/put! a-chan in1) (async/put! a-chan 0)
    (async/put! b-chan in2)
    (async/put! c-chan in3)
    (async/put! d-chan in4)
    (async/put! e-chan in5)

    (day7-build-program program a-chan b-chan)
    (day7-build-program program b-chan c-chan)
    (day7-build-program program c-chan d-chan)
    (day7-build-program program d-chan e-chan)
    (day7-build-program program e-chan a-chan)
    a-chan))

(defn get-all-permutations [start finish]
  (let [codes (range start finish)]
    (for [a codes
          b (remove #{a} codes)
          c (remove #{a b} codes)
          d (remove #{a b c} codes)
          e (remove #{a b c d} codes)]
      [a b c d e])))

(defn try-all-combinations [program code-combos]
  (async/merge
   (doall
    (for [combo code-combos]
      (main-orchestrator program combo)))
   (inc (count code-combos))))

(comment
  (do
    (let [outputs (try-all-combinations root-program (get-all-permutations 5 10))]
      (async/<!! (async/timeout 3000))
      (async/close! outputs)
      (apply max (async/<!! (async/into [] outputs))))))
  ;; => 6489132

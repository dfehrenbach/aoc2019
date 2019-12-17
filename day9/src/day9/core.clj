(ns day9.core
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [chan <!! >!! <! >!]]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as px]))

(def root-program (mapv
                   #(Integer/parseInt %)
                   (str/split (slurp "resources/input.txt") #",|\r\n|\n")))

(defn mapify-program [program]
  (reduce (fn [acc v] (assoc acc (count acc) (bigint v))) {} program))

(def mapped-root (mapify-program root-program))

(defn get-block [mapped-program index]
  [(mapped-program index)
   (mapped-program (+ 1 index))
   (mapped-program (+ 2 index))
   (mapped-program (+ 3 index))])

(defn opcode-digits [opcode]
  (let [[op1 op10 m1 m2 m3] (reverse (str opcode))]
    (->> [(str/join [op10 op1]) m1 m2 m3]
         (map str)
         (map #(if (empty? %) "0" %))
         (map #(Integer/parseInt %)))))

(defn get-param-value [program relative-base mode param]
  (let [pos-overflow? (<= (count program) param)]
    (case mode
      1 param
      2 (or (get program (+ param relative-base)) (when pos-overflow? 0))
      (or (get program param) (when pos-overflow? 0)))))

(defn day9-build-program [starting-program inchan outchan]
  (async/go-loop [program starting-program
                  program-index 0
                  relative-base 0]
    (let
     [[op' p1' p2' p3] (get-block program program-index)
      [op m1 m2 m3] (opcode-digits op')
      p1 (get-param-value program relative-base m1 p1')
      p2 (get-param-value program relative-base m2 p2')]

      (case op
        1 (recur (assoc program p3 (+ p1 p2))
                 (+ 4 program-index)
                 relative-base)

        2 (recur (assoc program p3 (* p1 p2))
                 (+ 4 program-index)
                 relative-base)

        3 (recur (assoc program p1' (async/<! inchan))
                 (+ 2 program-index)
                 relative-base)

        4 (do
            (async/>! outchan p1)
            (recur program
                   (+ 2 program-index)
                   relative-base))

        5 (recur program
                 (if (zero? p1) (+ 3 program-index) p2)
                 relative-base)

        6 (recur program
                 (if (zero? p1) p2 (+ 3 program-index))
                 relative-base)

        7 (recur (assoc program p3 (if (< p1 p2) 1 0))
                 (+ 4 program-index)
                 relative-base)

        8 (recur (assoc program p3 (if (= p1 p2) 1 0))
                 (+ 4 program-index)
                 relative-base)

        9 (recur program
                 (+ 2 program-index)
                 (+ relative-base p1))

        99 :exit

        0 (throw "WUT?!")))))

#_(defn main-orchestrator [program input]
    (let [a-chan (chan (async/sliding-buffer 2))
          b-chan (chan (async/sliding-buffer 2))
          c-chan (chan (async/sliding-buffer 2))
          d-chan (chan (async/sliding-buffer 2))
          e-chan (chan (async/sliding-buffer 2))
          [in1 in2 in3 in4 in5] input]

      (async/put! a-chan in1) (async/put! a-chan 0)
      (async/put! b-chan in2)
      (async/put! c-chan in3)
      (async/put! d-chan in4)
      (async/put! e-chan in5)

      (day9-build-program program a-chan b-chan)
      (day9-build-program program b-chan c-chan)
      (day9-build-program program c-chan d-chan)
      (day9-build-program program d-chan e-chan)
      (day9-build-program program e-chan a-chan)
      a-chan))

#_(defn get-all-permutations [start finish]
    (let [codes (range start finish)]
      (for [a codes
            b (remove #{a} codes)
            c (remove #{a b} codes)
            d (remove #{a b c} codes)
            e (remove #{a b c d} codes)]
        [a b c d e])))

#_(defn try-all-combinations [program code-combos]
    (async/merge
     (doall
      (for [combo code-combos]
        (main-orchestrator program combo)))
     (inc (count code-combos))))

(defn run-day9-part1 [program input]
  (let [inchan (chan 2000)
        outchan (chan 2000)]
    (async/put! inchan input)
    (day9-build-program program inchan outchan)
    outchan))

(comment
  #_(do
      (let [outputs (try-all-combinations root-program (get-all-permutations 5 10))]
        (async/<!! (async/timeout 3000))
        (async/close! outputs)
        (apply max (async/<!! (async/into [] outputs)))))
  ;; => 6489132
  (do
    (let [outputs (run-day9-part1 (mapify-program [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]) 1)]
      (async/<!! (async/timeout 1000))
      (async/close! outputs)
      (async/<!! (async/into [] outputs))))
  ;; => [109N 1N 204N -1N 1001N 100N 1N 100N 1008N 100N 16N 101N 1006N 101N 0N 99N]

  (do
    (let [outputs (run-day9-part1 (mapify-program [1102 34915192 34915192 7 4 7 99 0]) 1)]
      (async/<!! (async/timeout 1000))
      (async/close! outputs)
      (async/<!! (async/into [] outputs))))
  ;; => [1219070632396864N]

  (do
    (let [outputs (run-day9-part1 (mapify-program [104 1125899906842624 99]) 1)]
      (async/<!! (async/timeout 1000))
      (async/close! outputs)
      (async/<!! (async/into [] outputs))))
  ;; => [1125899906842624N]

  (do
    (let [outputs (run-day9-part1 (mapify-program [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]) 10)]
      (async/<!! (async/timeout 1000))
      (async/close! outputs)
      (async/<!! (async/into [] outputs))))

  (do
    (let [outputs (run-day9-part1 mapped-root 1)]
      (async/<!! (async/timeout 1000))
      (async/close! outputs)
      (async/<!! (async/into [] outputs)))))

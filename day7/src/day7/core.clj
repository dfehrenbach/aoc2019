(ns day7.core
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [chan <!! >!! <! >!]]))

(def root-program (mapv
                   #(Integer/parseInt %)
                   (str/split (slurp "resources/input.txt") #",|\r\n")))

(def !final-output (atom 0))

(defn day7-update-program [chunk acc program-index inchan outchan !final-out]
  (let [[opcode _ m1 m2] (map #(Integer/parseInt (str %)) (reverse (str (first chunk))))
        [p1 p2 p3] (rest chunk)]
    (case opcode
      1 (let [p1 (if (pos-int? m1) p1 (get acc p1))
              p2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog (assoc acc p3 (+ p1 p2))
           :prog-index (+ 4 program-index)})

      2 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog (assoc acc p3 (* v1 v2))
           :prog-index (+ 4 program-index)})

      3 (let [input (<!! inchan)]
          {:prog (assoc acc p1 input)
           :prog-index (+ 2 program-index)}) ;; Input 1 to start with

      4 (let [v1 (if (pos-int? m1) p1 (get acc p1))]
          (reset! !final-out v1)
          (async/go (>! outchan v1))
          #_(async/put! outchan v1)
          {:prog acc
           :prog-index (+ 2 program-index)})

      5 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog acc
           :prog-index (if (not (zero? v1)) v2
                           (+ 3 program-index))})

      6 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog acc
           :prog-index (if (zero? v1) v2
                           (+ 3 program-index))})

      7 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog (if (< v1 v2) (assoc acc p3 1) (assoc acc p3 0))
           :prog-index (+ 4 program-index)})

      8 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          {:prog (if (= v1 v2) (assoc acc p3 1) (assoc acc p3 0))
           :prog-index (+ 4 program-index)})

      (throw (println "ERROR WITH OPCODE:" opcode)))))

(defn day7-build-program [starting-program inchan outchan !final-out final-chan]
  (loop [acc starting-program
         program-index 0]
    (let [remaining-program (vec (drop program-index acc))
          next-op-code (Integer/parseInt (str (last (str (first remaining-program)))))
          instruction-length (case next-op-code
                               1 4, 2 4
                               3 2, 4 2
                               5 3, 6 3
                               7 4, 8 4
                               9 0, (throw (println "ERROR FOR NEXT OP CODE: " next-op-code)))
          chunk (take instruction-length remaining-program)]
      (if (= 99 (first remaining-program))
        (async/put! final-chan (deref !final-out))
        (let [{updated-program :prog
               next-program-index :prog-index}
              (day7-update-program chunk acc program-index inchan outchan !final-out)]
          (recur updated-program
                 next-program-index))))))

(defn main-orchestrator [program input]
  (let [a-chan (chan (async/sliding-buffer 2))
        b-chan (chan (async/sliding-buffer 2))
        c-chan (chan (async/sliding-buffer 2))
        d-chan (chan (async/sliding-buffer 2))
        e-chan (chan (async/sliding-buffer 2))
        final-chan (chan (async/sliding-buffer 5))
        !final-out (atom 0)
        [in1 in2 in3 in4 in5] input]

    (async/go (>! a-chan in1) (>! a-chan 0)
              (day7-build-program program a-chan b-chan !final-out final-chan))
    (async/go (>! b-chan in2)
              (day7-build-program program b-chan c-chan !final-out final-chan))
    (async/go (>! c-chan in3)
              (day7-build-program program c-chan d-chan !final-out final-chan))
    (async/go (>! d-chan in4)
              (day7-build-program program d-chan e-chan !final-out final-chan))
    (async/go (>! e-chan in5)
              (day7-build-program program e-chan a-chan !final-out final-chan))

    (loop [count 0]
      (if (= count 5)
        (deref !final-out)
        (do
          (println "From final: " (<!! final-chan))
          (recur (inc count)))))))

(defn get-all-combinations [start finish]
  (let [codes (range start finish)]
    (for [a codes
          b (remove #{a} codes)
          c (remove #{a b} codes)
          d (remove #{a b c} codes)
          e (remove #{a b c d} codes)]
      [a b c d e])))

(defn try-all-combinations-part2 [program code-combos]
  (reduce (fn [highval combo]
            (println "combo: " combo "highestval: " highval)
            (max highval (main-orchestrator program combo))) 0 code-combos))

(comment
  #_(apply max-key :output (try-all-combinations root-program))
  ;; => {:combo [3 1 2 0 4], :output 14902} ;; PART 1 Answer

  (try-all-combinations-part2 [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
                               27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                              (get-all-combinations 5 10))
  ;; => 141794532


  (main-orchestrator root-program [9 8 7 6 5])
  ;; => 4056746

  (try-all-combinations-part2 root-program (take 60 (get-all-combinations 5 10)))
  ;; => 6472876

  (try-all-combinations-part2 root-program (drop 60 (get-all-combinations 5 10)))
  ;; => 6489132
  0)

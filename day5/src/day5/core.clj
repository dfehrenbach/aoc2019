(ns day5.core
  (:require [clojure.string :as str]))

(def root-program (mapv
                   #(Integer/parseInt %)
                   (str/split (slurp "resources/input.txt") #",|\r\n")))

(def program-input (atom 5))
(def final-output (atom 0))

(defn day5-update-program [chunk acc program-index]
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

      3 {:prog (assoc acc p1 (deref program-input))
         :prog-index (+ 2 program-index)} ;; Input 1 to start with

      4 (let [v1 (if (pos-int? m1) p1 (get acc p1))]
          (swap! final-output (fn [_] v1))
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

(defn day5-build-program [starting-program]
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
      (if (= 99 (first remaining-program)) acc
          (let [{updated-program :prog
                 next-program-index :prog-index}
                (day5-update-program chunk acc program-index)]
            (recur updated-program
                   next-program-index))))))

(comment
  (swap! program-input (fn [_] 1))
  (day5-build-program root-program)
  (deref final-output)
;; => 72598358

  (swap! program-input (fn [_] 5))
  (day5-build-program root-program)
  (deref final-output))
;; => 11826654

(ns day5.core
  (:require [clojure.string :as str]))

(def root-program (mapv
                   #(Integer/parseInt %)
                   (str/split (slurp "resources/input.txt") #",|\r\n")))

; DAY 5 Part 1


(Integer/parseInt "1")

(comment
  (map #(Integer/parseInt (str %)) "1011"))

(def program-input 1)

(defn day5-update-program [c acc]
  (let [[opcode _ m1 m2] (map #(Integer/parseInt (str %)) (reverse (str (first c))))
        [p1 p2 p3] (rest c)]
    (case opcode
      1 (let [p1 (if (pos-int? m1) p1 (get acc p1))
              p2 (if (pos-int? m2) p2 (get acc p2))]
          (assoc acc p3 (+ p1 p2)))

      2 (let [v1 (if (pos-int? m1) p1 (get acc p1))
              v2 (if (pos-int? m2) p2 (get acc p2))]
          (assoc acc p3 (* v1 v2)))

      3 (assoc acc p1 program-input) ;; Input 1 to start with

      4 (let [v1 (if (pos-int? m1) p1 (get acc p1))]
          (println v1)
          acc)

      (throw (println "ERROR WITH OPCODE:" opcode)))))

(defn extract-next-opcode [program]
  (Integer/parseInt (str (last (str (first program))))))

(defn day5-build-program [starting-program]
  (loop [acc starting-program
         program-index 0]
    (let [remaining-program (vec (drop program-index acc))
          instruction-length (if (or (= (extract-next-opcode remaining-program) 3)
                                     (= (extract-next-opcode remaining-program) 4)) 2 4)
          chunk (take instruction-length remaining-program)]

      (if (= 99 (first remaining-program)) acc
          (recur (day5-update-program chunk acc)
                 (+ instruction-length program-index))))))

(day5-build-program root-program)

(comment
  (def restprogram (vec (drop 2 root-program)))
  (def thing (take 4 restprogram))

 ;; => (1 225 6 6)

  (get (vec restprogram) 1)

  (day5-update-program thing restprogram)

  (day5-update-program (take 4 (drop 2 root-program)) (drop 2 root-program))

  (let [[opcode _ m1 m2 m3] (reverse (str (first '(3 233))))]
    [opcode m1 m2 m3])

  (day5-build-program root-program)

  (reduce * (repeat 14 0.5)))

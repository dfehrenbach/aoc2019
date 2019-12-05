(ns day2.core
  (:require [clojure.string :as str]))

(def program (mapv
              #(Integer/parseInt %)
              (str/split (slurp "resources/input.txt") #",|\r\n")))

(def program-alarm (-> program
                       (assoc 1 12)
                       (assoc 2 2)))


; Part 1 


(defn update-program [[opcode in1 in2 placement] acc]
  (println {:op opcode :in1 in1 :in2 in2 :placement placement})
  (let [operation (case opcode 1 + 2 *)
        [val1 val2] [(get acc in1) (get acc in2)]
        new-val (operation val1 val2)]
    (assoc acc placement new-val)))

(defn build-program [starting-program]
  (loop [acc starting-program
         program-index 0]
    (let [[chunk & _] (partition-all 4 (drop program-index acc))]
      (println {:chunk chunk})
      (if (= 99 (first chunk)) acc
          (let [updated (update-program chunk acc)]
            (recur updated
                   (+ 4 program-index)))))))

(first (build-program program-alarm))
;; => [5290681 12 2 2 1 1 2 3 1 3 4 3 1 5 0 3 2 6 1 24 1 5 19 25 1 13 23 
;;     30 1 6 27 32 2 31 13 160 1 9 35 163 2 39 13 815 1 43 10 819 1 47 
;;     13 824 2 13 51 4120 1 55 9 4123 1 59 5 4124 1 6 63 4126 1 13 67 
;;     4131 2 71 10 16524 1 6 75 16526 1 79 10 16530 1 5 83 16531 2 10 
;;     87 66124 1 6 91 66126 1 9 95 66129 1 99 9 66132 2 103 10 264528 
;;     1 5 107 264529 1 9 111 264532 2 13 115 1322660 1 119 10 1322664 
;;     1 123 10 1322668 2 127 10 5290672 1 5 131 5290673 1 10 135 5290677 
;;     1 139 2 5290679 1 6 143 0 99 2 14 0 0]

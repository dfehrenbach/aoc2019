(ns day13.core2
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [chan <!! >!! <! >!]]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]))

(def root-program (mapv
                    #(Long/parseLong %)
                    (str/split (slurp "resources/day13/input.txt") #",|\r\n|\n")))

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
  (case mode
    1 param
    2 (get program (+ param relative-base) 0)
    (get program param 0)))

(defn fresh-program [program]
  {:program       program
   :program-index 0
   :relative-base 0
   :inputs        []
   :outputs       []
   :exitted       false
   :waiting       false})

(defn build-program [{:keys [program program-index relative-base inputs outputs]
                      :as   computer}]
  (let [[op' p1' p2' p3'] (get-block program program-index)
        [op m1 m2 m3]     (opcode-digits op')
        p1                (get-param-value program relative-base m1 p1')
        p2                (get-param-value program relative-base m2 p2')
        p3                (if (= m3 2) (+ relative-base p3') p3')]
    (case op
      1 (-> computer
            (assoc-in [:program p3] (+ p1 p2))
            (assoc :program-index (+ program-index 4)))

      2 (-> computer
            (assoc-in [:program p3] (* p1 p2))
            (assoc :program-index (+ program-index 4)))

      3 (if (empty? inputs) (assoc computer :waiting true)
            (-> computer
                (assoc-in [:program (if (= m1 2) (+ relative-base p1') p1')] (first inputs))
                (assoc :program-index (+ program-index 2))
                (assoc :waiting false)
                (assoc :inputs (rest inputs))))

      4 (-> computer
            (assoc :outputs (conj outputs p1))
            (assoc :program-index (+ program-index 2)))

      5 (-> computer
            (assoc :program-index (if (zero? p1) (+ 3 program-index) p2)))

      6 (-> computer
            (assoc :program-index (if (zero? p1) p2 (+ 3 program-index))))

      7 (-> computer
            (assoc-in [:program p3] (if (< p1 p2) 1 0))
            (assoc :program-index (+ program-index 4)))

      8 (-> computer
            (assoc-in [:program p3] (if (= p1 p2) 1 0))
            (assoc :program-index (+ program-index 4)))

      9 (-> computer
            (assoc :program-index (+ program-index 2))
            (assoc :relative-base (+ relative-base p1)))

      99 (assoc computer :exitted true)

      0 :weird)))

(def fresh-game-state
  {:game-map     {}
   :printed-game (mapv vec (make-array Integer/TYPE 41 41))
   :score        0})

(defn ids->pictures [printed-game]
  (map (fn [line]
         (reduce str (map (fn [id]
                            (case id
                              0 " " 1 "█" 2 "░" 3 "─" 4 "■"))
                          line)))
       printed-game))

(defn print-game [state]
  (ids->pictures (:printed-game state)))

(defn update-game [game x y id]
  (if (= [x y] [-1 0]) (assoc game :score id)
      (-> game
          (assoc-in [:game-map [x y]] id)
          (assoc-in [:printed-game y x] id))))

(defn run-program [program]
  (if (or (:exitted program) (:waiting program)) program
      (recur (build-program program))))

(defn part1 [program input]
  (let [starting-program    (assoc (fresh-program program) :inputs (concat [] input))
        setup-program-state (run-program starting-program)
        game-instructions   (partition-all 3 (:outputs setup-program-state))
        setup-game          (reduce (fn [acc [x y id]]
                                      (update-game acc x y id))
                                    fresh-game-state
                                    game-instructions)
        all-blocks          (get (group-by val (:game-map setup-game)) 2)]
    (count all-blocks)))

(defn build-upon-game [game instructions]
  (reduce (fn [acc [x y id]]
            (update-game acc x y id))
          game
          instructions))

(defn get-ball-position [game]
  (ffirst (get (group-by val (:game-map game)) 4)))

(defn get-paddle-position [game]
  (ffirst (get (group-by val (:game-map game)) 3)))

(defn iterate-program-game [program game movement]
  (let [next-program (-> program
                         (assoc :outputs [])
                         (update :inputs conj movement)
                         (assoc :waiting false)
                         run-program)
        instructions (partition-all 3 (:outputs next-program))
        next-game    (build-upon-game game instructions)]
    {:program next-program
     :game    next-game}))

(defn find-paddle-movement [paddle ball future-ball]
  (let [[px py]   paddle
        [bx by]   ball
        [fbx fby] future-ball]
    (if (and (= px bx) (= py (inc by))) 0
        (compare fbx px))))

(defn progress-game [program game]
  (let [current-paddle-position (get-paddle-position game)
        current-ball-position   (get-ball-position game)
        future-ball             (get-ball-position
                                  (:game (iterate-program-game program game 0)))
        true-movement           (find-paddle-movement current-paddle-position
                                                      current-ball-position
                                                      future-ball)]
    (iterate-program-game program game true-movement)))

(defn run-part2 [setup-program setup-game]
  (loop [program setup-program
         game    setup-game]
    (let [{:keys [program game] :as data} (progress-game program game)]
      (if (:exitted program) data
          (recur program game)))))

(defn part2 [program input]
  (let [starting-program  (assoc (fresh-program program) :inputs (concat [] input))
        setup-program     (run-program starting-program)
        game-instructions (partition-all 3 (:outputs setup-program))
        setup-game        (build-upon-game fresh-game-state game-instructions)]

    (run-part2 setup-program setup-game)))

(comment
  (part1 mapped-root [0])
  ;; => 420

  (:score (:game (part2 (assoc mapped-root 0 2N) [])))
  ;; => 21651N

  (pm/logs)

  (pm/reset!)

  0)



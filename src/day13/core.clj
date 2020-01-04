(ns day13.core
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

(defn day13-build-program
  ([starting-program inchan outchan resultchan]
   (day13-build-program starting-program 0 0 inchan outchan resultchan))
  ([starting-program starting-index starting-base inchan outchan resultchan]
   (async/go-loop [program starting-program
                   program-index starting-index
                   relative-base starting-base]
     (let
         [[op' p1' p2' p3'] (get-block program program-index)
          [op m1 m2 m3]     (opcode-digits op')
          p1                (get-param-value program relative-base m1 p1')
          p2                (get-param-value program relative-base m2 p2')
          p3                (if (= m3 2) (+ relative-base p3') p3')]

       (pm/dump :penguin (map (fn [m] (select-keys m [:op' :p1 :p2 :p3 :program-index]))))
       (case op

         1 (recur (assoc program p3 (+ p1 p2))
                  (+ 4 program-index)
                  relative-base)

         2 (recur (assoc program p3 (* p1 p2))
                  (+ 4 program-index)
                  relative-base)

         3 (do (pm/spy>> :three "entered 3 will await input")
               (recur (assoc program (if (= m1 2) (+ relative-base p1') p1')
                             (pm/spy>> :program-in (async/<! inchan)))
                      (+ 2 program-index)
                      relative-base))

         4 (do
             (async/>! outchan (pm/spy>> :program-out p1))
             (async/>! resultchan [program program-index relative-base])
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

         99 (do (async/>! resultchan program program-index relative-base)
                :exit)

         0 (throw "WUT?!"))))))

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

(defn update-game [game x y id]
  (pm/spy>> :picture (ids->pictures (:printed-game game)))
  (if (= x -1) (assoc game :score y)
      (-> game
          (assoc-in [:game-map [x y]] id)
          (assoc-in [:printed-game y x] id))))

(defn game
  ([in final]
   (game fresh-game-state in final))
  ([starting-state in final]
   (async/go-loop [game starting-state]
     (let [[x y id]        [(async/<! in) (async/<! in) (async/<! in)]
           next-game-state (update-game game x y id)]
       (async/>! final next-game-state)
       (recur next-game-state)))))


;; we start program-in (send nothing)
;; it runs passing output to game-in
;;   game-in slurps in groups of 3 (x y id)
;;   it builds a new game-state
;;   passing that to the sliding-buffer 1 finally
;;   it loops, waiting for 3 more inputs
;; it also saves last known program state on every output
;; when we come to accepting inputs, we use what's available and provide more outputs
;;


(defn run-day13-part1 [program input]
  (let [program-in    (chan (async/sliding-buffer 100))
        program-final (chan (async/sliding-buffer 1))
        game-in       (chan (async/sliding-buffer 100))
        game-final    (chan (async/sliding-buffer 1))]
    (async/put! game-final fresh-game-state)
    (async/onto-chan program-in input)

    (day13-build-program program program-in game-in program-final)
    (game game-in game-final)

    [program-in game-final]))

(defn part1 [program input]
  (let [[program-in game-out] (run-day13-part1 program input)]
    (async/<!! (async/timeout 1000))
    (async/close! game-out)
    (let [game-state (:game-map (last (async/<!! (async/into [] game-out))))
          all-blocks (get (group-by val game-state) 2)]
      (count all-blocks))))

(defn print-game [state]
  (ids->pictures (:printed-game state)))

(defn part2 [program input]
  (let [[_ game-out] (run-day13-part1 program input)]
    (async/<!! (async/timeout 2000))
    (async/close! game-out)
    (let [game-state (last (async/<!! (async/into [] game-out)))]
      (print-game game-state)
      #_(print-game (update-game game-state 12N 23N 4N)))))

(comment

  (part1 mapped-root 0)
                                        ; => 420

  (part2 (assoc mapped-root 0 2N) [1 0 0 0 0])

  (pm/logs)

  (pm/reset!)

  0)


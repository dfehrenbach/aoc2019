(ns day11.core
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [chan <!! >!! <! >!]]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]))

(def root-program (mapv
                   #(Long/parseLong %)
                   (str/split (slurp "resources/day11/input.txt") #",|\r\n|\n")))

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

(defn day11-build-program [starting-program inchan outchan]
  (async/go-loop [program starting-program
                  program-index 0
                  relative-base 0]
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

        3 (recur (assoc program (if (= m1 2) (+ relative-base p1') p1')
                        (pm/spy>> :program-in (async/<! inchan)))
                 (+ 2 program-index)
                 relative-base)

        4 (do
            (async/>! outchan (pm/spy>> :program-out p1))
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

(def fresh-robot-state
  {:position  [0 0]
   :direction :up
   :path      {[0 0] 0}})

(defn find-current-color [robot]
  (get (:path robot) (:position robot) 0))

(defn paint [color robot]
  (assoc-in robot [:path (:position robot)] color))

(defn turn [direction robot]
  (let [new-direction (condp = [(:direction robot) direction]
                        [:up 1]    :right [:up 0]    :left
                        [:right 1] :down  [:right 0] :up
                        [:down 1]  :left  [:down 0]  :right
                        [:left 1]  :up    [:left 0]  :down)]
    (assoc robot :direction new-direction)))

(defn move [robot]
  (let [update-position (case (:direction robot)
                          :up    (fn [[x y]] [x (inc y)])
                          :right (fn [[x y]] [(inc x) y])
                          :down  (fn [[x y]] [x (dec y)])
                          :left  (fn [[x y]] [(dec x) y]))]
    (update robot :position update-position)))

(defn robot [in out final]
  (async/go-loop [robot fresh-robot-state]
    (let [[color rotation] [(async/<! in) (async/<! in)]
          next-robot-state (->> robot (paint color) (turn rotation) move)]
      (async/>! out (bigint (find-current-color next-robot-state)))
      (async/>! final next-robot-state)
      (recur next-robot-state))))

(defn syncRobot [steps]
  (loop [robot    fresh-robot-state
         accsteps steps]
    (if (nil? (first accsteps)) robot
        (let [[color rotation] (first accsteps)
              next-robot-state (->> robot (paint color) (turn rotation) move)]
          (recur next-robot-state
                 (rest accsteps))))))

(comment
  (count (:path (syncRobot [[1 0] [0 0] [1 0] [1 0] [0 1] [1 0] [1 0]]))))

(defn run-day11-part1 [program input]
  (let [inchan      (chan 2000)
        robot-in    (chan 2000)
        robot-final (chan (async/sliding-buffer 1))]
    (async/put! inchan input)
    (day11-build-program program inchan robot-in)
    (robot robot-in inchan robot-final)
    robot-final))

(defn part1 [program input]
  (let [outputs (run-day11-part1 program input)]
    (async/<!! (async/timeout 5000))
    (async/close! outputs)
    (let [allrobots (async/<!! (async/into [] outputs))]
      (count (:path (last allrobots))))))

(defn rising-x-falling-y [x y]
  (let [[[x1 y1] _] x
        [[x2 y2] _] y]
    (compare [y2 x1]
             [y1 x2])))

(defn nums->blocks [num]
  (if (zero? num) "·" "▓"))

(defn part2 [program input]
  (let [outputs (run-day11-part1 program input)]
    (async/<!! (async/timeout 1000))
    (async/close! outputs)
    (let [lastrobots          (async/<!! (async/into [] outputs))
          sorted-path         (sort rising-x-falling-y (:path (last lastrobots)))
          square-painting-fns (map (fn [[[x y] color]]
                                     (fn [hull] (assoc-in hull [(Math/abs (int y)) (Math/abs (int x))] (int color))))
                                   sorted-path)
          [minx maxx]         [(apply max (map ffirst sorted-path)) (apply min (map ffirst sorted-path))]
          [miny maxy]         [(apply max (mapcat nfirst sorted-path)) (apply min (mapcat nfirst sorted-path))]
          space-hull          (mapv vec (make-array Integer/TYPE
                                                    (inc (Math/abs (- maxy miny)))
                                                    (inc (Math/abs (- maxx minx)))))
          painted-hull        ((apply comp square-painting-fns) space-hull)]
      (map #(->> %
                 (map nums->blocks)
                 (reduce str))
           painted-hull))))

(comment
  ;; PART 1
  (part1 mapped-root 0)
  ;; => 2883
  
  (part2 mapped-root 1)
  ;; => ("·▓····▓▓▓▓·▓▓▓···▓▓··▓▓▓··▓·····▓▓··▓▓▓▓···"
  ;;     "·▓····▓····▓··▓·▓··▓·▓··▓·▓····▓··▓····▓···"
  ;;     "·▓····▓▓▓··▓··▓·▓····▓··▓·▓····▓······▓····"
  ;;     "·▓····▓····▓▓▓··▓····▓▓▓··▓····▓·▓▓··▓·····"
  ;;     "·▓····▓····▓····▓··▓·▓····▓····▓··▓·▓······"
  ;;     "·▓▓▓▓·▓▓▓▓·▓·····▓▓··▓····▓▓▓▓··▓▓▓·▓▓▓▓···")
  
  (pm/logs)
  (pm/reset!)

  0)

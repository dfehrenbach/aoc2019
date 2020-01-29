(ns day17.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [day13.core2 :as day13]))

(def root-program (mapv
                    #(Long/parseLong %)
                    (str/split (slurp "resources/day17/input.txt") #",|\r\n|\n")))

(defn mapify-program [program]
  (reduce (fn [acc v] (assoc acc (count acc) (bigint v))) {} program))

(def mapped-root (mapify-program root-program))

(defn mapify-output [scaffold]
  (apply merge
         (mapcat (fn [row yidx]
                   (map (fn [char xidx]
                          {[xidx yidx] char})
                        row (range (count row))))
                 scaffold (range (count scaffold)))))

(defn scaffold? [mapped-scaffold point]
  (= \# (get mapped-scaffold point)))

(defn intersection? [mapped-scaffold [point _]]
  (let [[x y] point
        up    [x (+ y 1)]
        down  [x (- y 1)]
        left  [(+ x 1) y]
        right [(+ x 1) y]]
    (pm/dump :otter)
    (every? (partial scaffold? mapped-scaffold) [point up down left right])))

(defn find-alignment-param [[[x y] _]] (* x y))

(defn build-scaffold [program-outputs]
  (mapify-output
    (str/split-lines
      (apply str (map char (:outputs program-outputs))))))

(defn part1 [input]
  (let [starting-program (day13/fresh-program input)
        mapped-scaffold  (build-scaffold (day13/run-program starting-program))
        intersections    (filter (partial intersection? mapped-scaffold) mapped-scaffold)]
    (reduce + (map find-alignment-param intersections))))

(defn find-bot [scaffold]
  {:direction    :up
   :position     (key (first (filter #(= \^ (val %)) scaffold)))
   :instructions []})

(defn turn-bot [scaffold bot]
  (let [{direction   :direction
         [botx boty] :position} bot
        [up down]               [[botx (dec boty)] [botx (inc boty)]]
        [left right]            [[(dec botx) boty] [(inc botx) boty]]
        scaffold?               (partial scaffold? scaffold)
        instruction             (case direction
                                  :up    (cond (scaffold? left)  \L
                                               (scaffold? right) \R)
                                  :down  (cond (scaffold? left)  \R
                                               (scaffold? right) \L)
                                  :left  (cond (scaffold? up)   \R
                                               (scaffold? down) \L)
                                  :right (cond (scaffold? up)   \L
                                               (scaffold? down) \R))
        new-direction           (case [direction instruction]
                                  [:up \L]    :left  [:up \R]    :right
                                  [:down \L]  :right [:down \R]  :left
                                  [:left \L]  :down  [:left \R]  :up
                                  [:right \L] :up    [:right \R] :down
                                  :stop)]
    (if (= new-direction :stop) bot
        (-> bot
            (assoc :direction new-direction)
            (update :instructions conj instruction)))))

(defn move-bot [scaffold bot]
  (let [{direction   :direction
         [botx boty] :position}     bot
        [maxScaffoldx maxScaffoldy] [(apply max (map first (keys scaffold)))
                                     (apply max (map second (keys scaffold)))]

        path-infront-of-bot     (case direction
                                  :up    {:xs [botx] :ys (drop 1 (range boty (dec 0) -1))}         ;; Bot is looking "up" but sees "lower" y values
                                  :down  {:xs [botx] :ys (drop 1 (range boty (inc maxScaffoldy)))} ;; Bot is looking "down" but sees "higher" y values
                                  :left  {:ys [boty] :xs (drop 1 (range botx (dec 0) -1))}
                                  :right {:ys [boty] :xs (drop 1 (range botx (inc maxScaffoldx)))})
        scaffold-infront-of-bot (for [x (:xs path-infront-of-bot)
                                      y (:ys path-infront-of-bot)]
                                  (get scaffold [x y]))
        instruction             (count (take-while #(= % \#) scaffold-infront-of-bot))
        new-position            (case direction
                                  :up    [botx (- boty instruction)]
                                  :down  [botx (+ boty instruction)]
                                  :left  [(- botx instruction) boty]
                                  :right [(+ botx instruction) boty])]
    (if (= new-position :stop) bot
        (-> bot
            (assoc :position new-position)
            (update :instructions conj instruction)))))

(defn run-part2 [scaffold bot]
  (pm/dump :penguin)
  (if (= 0 (last (:instructions bot))) (update bot :instructions (partial drop-last 1))
      (recur scaffold
             (->> bot
                  (turn-bot scaffold)
                  (move-bot scaffold)))))

(defn subseq [a b]
  (some #{a} (drop 1 (partition (count a) 1 b))))

(drop 1 (reductions conj [] [1 2 3 4 5]))

(defn find-subroutine [instructions]
  (take-while (fn [instruction-group]
                (subseq instruction-group instructions))
              (drop 1 (reductions conj [] instructions))))

(defn part2 [input]
  (let [starting-program  (day13/fresh-program input)
        mapped-scaffold   (build-scaffold (day13/run-program starting-program))
        starting-bot      (find-bot mapped-scaffold)
        path-instructions (partition 2 (:instructions (run-part2 mapped-scaffold starting-bot)))]
    {:full    path-instructions
     :working (filter #(< 1 (val %))(frequencies (partition 3 path-instructions)))
     :sub     (find-subroutine path-instructions)}

    #_(->> starting-bot (turn-bot mapped-scaffold) (move-bot mapped-scaffold))
    #_[(turn-bot mapped-scaffold starting-bot)
       (move-bot mapped-scaffold (turn-bot mapped-scaffold starting-bot))]))

(comment

  (part1 mapped-root)
  ;; => 2788

  (part2 mapped-root)

  (pm/logs)

  (pm/reset!)

  (def fresh-environment
    {:bot-position {:x 0 :y 0}
     :map          {[0 0] :nothing}
     :path         []})

  (defn go-direction [program direction]
    (day13/run-program
      (-> program
          (update :inputs conj direction)
          (assoc :waiting false)))))

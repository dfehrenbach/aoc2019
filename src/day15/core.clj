(ns day15.core
  (:require [clojure.string :as str]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]
            [day13.core2 :as day13]))

(def root-program (mapv
                    #(Long/parseLong %)
                    (str/split (slurp "resources/day15/input.txt") #",|\r\n|\n")))

(defn mapify-program [program]
  (reduce (fn [acc v] (assoc acc (count acc) (bigint v))) {} program))

(def mapped-root (mapify-program root-program))

(def fresh-environment
  {:bot-position {:x 0 :y 0}
   :map          {[0 0] :nothing}
   :path         []})

(defn decide-val [existing-val new-val]
  (case [existing-val new-val]
    [nil 0]         :wall
    [nil 1]         :unexplored
    [nil 2]         :OXYGEN
    [:wall 0]       :wall
    [:nothing 1]    :nothing
    [:unexplored 1] :nothing
    [:OXYGEN 2]     :OXYGEN
    :error))

(defn update-direction [env location val]
  (let [existing-val (get-in env [:map location])
        new-val      (decide-val existing-val val)]
    (assoc-in env [:map location] new-val)))

(defn record-environment [env {:keys [north south east west]}]
  (let [{bot-x :x bot-y :y} (:bot-position env)]
    (-> env
        (update-direction [bot-x (inc bot-y)] north)
        (update-direction [bot-x (dec bot-y)] south)
        (update-direction [(dec bot-x) bot-y] west)
        (update-direction [(inc bot-x) bot-y] east))))

(defn go-direction [program direction]
  (day13/run-program
    (-> program
        (update :inputs conj direction)
        (assoc :waiting false))))

(defn try-all-directions [program env]
  (let [surrounding-area {:north (last (:outputs (go-direction program 1)))
                          :south (last (:outputs (go-direction program 2)))
                          :west  (last (:outputs (go-direction program 3)))
                          :east  (last (:outputs (go-direction program 4)))}]
    (record-environment env surrounding-area)))

(def direction->code {:north 1, :south 2, :west 3, :east 4})

(defn unexplored-directions [env]
  (let [{bot-x :x bot-y :y} (:bot-position env)
        surrounding-area    {:north (get-in env [:map [bot-x (inc bot-y)]] :unexplored)
                             :south (get-in env [:map [bot-x (dec bot-y)]] :unexplored)
                             :west  (get-in env [:map [(dec bot-x) bot-y]] :unexplored)
                             :east  (get-in env [:map [(inc bot-x) bot-y]] :unexplored)}
        unexplored-or-oxy   (fn [[_pos v]] (or (= v :unexplored) (= v :OXYGEN)))
        unexplored          (filter unexplored-or-oxy surrounding-area)]
    (map (comp direction->code key) unexplored)))

(defn move-bot [env direction]
  (let [{bot-x :x bot-y :y} (:bot-position env)]
    (case direction
      1 (-> env
            (update-in [:bot-position :y] inc)
            (update :path conj [bot-x (inc bot-y)]))
      2 (-> env
            (update-in [:bot-position :y] dec)
            (update :path conj [bot-x (dec bot-y)]))
      3 (-> env
            (update-in [:bot-position :x] dec)
            (update :path conj [(dec bot-x) bot-y]))
      4 (-> env
            (update-in [:bot-position :x] inc)
            (update :path conj [(inc bot-x) bot-y]))
      (throw "Invalid direciton sent to move-bot"))))

(defn find-oxygen-direction [env oxygen-pos]
  (let [{bot-x :x bot-y :y} (:bot-position env)
        [oxy-x oxy-y]       (val oxygen-pos)]
    (case [(compare oxy-x bot-x) (compare oxy-y bot-y)]
      [0 1]  1
      [0 -1] 2
      [-1 0] 3
      [1 0]  4)))

(defn found-oxygen? [env]
  (some #(when (= :OXYGEN (val %)) %) (:map env)))

(defn run-part1 [program env]
  (if (some? (found-oxygen? env)) {:program program :env env}
      (let [updated-env (try-all-directions program env)]
        (flatten (for [direction (unexplored-directions updated-env)]
                   (run-part1 (go-direction program direction)
                              (move-bot updated-env direction)))))))

(defn part1 [input]
  (let [starting-program (day13/fresh-program input)
        starting-env     fresh-environment]
    (first (run-part1 starting-program starting-env))))

(defn run-part2 [program env]
  (let [updated-env (try-all-directions program env)]
    (if (empty? (unexplored-directions updated-env)) (count (:path env))
        (flatten (for [direction (unexplored-directions updated-env)]
                   (run-part2 (go-direction program direction)
                              (move-bot updated-env direction)))))))

(defn refresh-env [env]
  (-> env
      (assoc :path [])
      (assoc :map
             (zipmap (keys (:map env))
                     (map #(case %
                             :wall       :wall
                             :OXYGEN     :nothing
                             :unexplored nil
                             :nothing    nil
                             :nil)
                          (vals (:map env)))))))

(defn part2 [input]
  (let [starting-program (day13/fresh-program input)
        starting-env     fresh-environment
        {program :program
         env     :env}   (first (run-part1 starting-program starting-env))]
    (run-part2 program (refresh-env env))))

(comment
  (-> (part1 mapped-root) :env :path count)
  ;; => 222

  (apply max (part2 mapped-root))
  ;; => 394

  (pm/logs)

  (pm/reset!)

  0)

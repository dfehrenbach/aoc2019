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
  (pm/dump :going-direction)
  (pm/spy>> :went-to (day13/run-program
                       (-> program
                           (update :inputs conj direction)
                           (assoc :waiting false)))))

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
        unexplored          (filter (comp (partial = :unexplored) val) surrounding-area)]
    (pm/dump :unexplored-directions)
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

(defn found-oxygen? [env]
  (some #{:OXYGEN} (vals (:map env))))

(defn run-part1 [program env]
  (pm/spy>> :program program)
  (let [updated-env (try-all-directions program env)]
    (pm/spy>> :pupper updated-env)
    (pm/spy>> :oxy? (some? (found-oxygen? updated-env)))
    (pm/spy>> :unexplored? [(:bot-position updated-env) (unexplored-directions updated-env)])
    (if (some? (found-oxygen? updated-env)) {:program program :env updated-env}
        (flatten (for [direction (unexplored-directions updated-env)]
                   (run-part1 (go-direction program direction)
                              (move-bot updated-env direction)))))))

(defn part1 [program]
  (let [starting-program (day13/fresh-program program)
        starting-env     fresh-environment]
    (first (run-part1 starting-program starting-env))))

(comment
  (filter (fn [[pos val]] (= val :OXYGEN))
          (:map (:env (part1 mapped-root))))
  ;; => ([[12 -14] :OXYGEN])

  ;; Increment because oxygen is NEXT to the robot
  (-> (part1 mapped-root) :env :path count inc)
  ;; => 222

  (pm/logs)

  (pm/reset!)

  0)

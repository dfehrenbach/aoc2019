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

(defn run-program [program]
  (if (or (:exitted program) (:waiting program)) program
      (recur (day13/build-program program))))

(def fresh-environment
  {:bot-position {:x 0 :y 0}
   :map          {[0 0] :nothing}
   :path         []})

(defn decide-val [existing-val new-val]
  (case [existing-val new-val]
    [nil 0]         :wall
    [nil 1]         :unexplored
    [nil 2]         :oxygen
    [:wall 0]       :wall
    [:nothing 1]    :nothing
    [:unexplored 1] :nothing
    [:oxygen 2]     :oxygen
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
        (update-direction [(inc bot-x) bot-y] east)
        (update-direction [(dec bot-x) bot-y] west))))

(defn go-direction [program direction]
  (last (:outputs (run-program (update program :inputs conj direction)))))

(defn try-all-directions [program env]
  (let [surrounding-area {:north (go-direction program 1)
                          :south (go-direction program 2)
                          :west  (go-direction program 3)
                          :east  (go-direction program 4)}]
    (record-environment env surrounding-area)))

(def direction->code {:north 1, :south 2, :west 3, :east 4})

(defn unexplored-directions [env]
  (let [{bot-x :x bot-y :y} (:bot-position env)
        surrounding-area    {:north (get-in env [:map [bot-x (inc bot-y)]])
                             :south (get-in env [:map [bot-x (dec bot-y)]])
                             :west  (get-in env [:map [(dec bot-x) bot-y]])
                             :east  (get-in env [:map [(inc bot-x) bot-y]])}
        unexplored          (filter (comp (partial = :unexplored) val) surrounding-area)]

    (pm/dump :elephant)
    (map (comp direction->code key) unexplored)))

(defn move-bot [env])

(defn run-part1 [program env])

(defn part1 [program input]
  (let [starting-program (assoc (fresh-program program) :inputs (concat [] input))
        starting-env     fresh-environment]
    (unexplored-directions (try-all-directions starting-program starting-env))))

(comment
  (part1 mapped-root [])

  (pm/logs)

  (pm/reset!)

  0)

(ns day14.core
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.set :refer [union intersection difference]]
            [postmortem.instrument :as pi]
            [postmortem.core :as pm]
            [postmortem.xforms :as xf]))

(def puzzle-input (slurp "resources/day14/input.txt"))

(defn mapify-ingredient-pair [[amount ingredient]]
  {(keyword ingredient) (bigint (Integer/parseInt amount))})

(defn transform-ingredients [ingredients]
(apply merge
       (map (comp
                mapify-ingredient-pair
                #(str/split % #" "))
              ingredients)))

(defn build-reactions [reactions [ingredients result]]
  (let [[amount result]    (str/split result #" ")
        ingredients        (str/split ingredients #", ")
        ingredient->amount (transform-ingredients ingredients)]
    (-> reactions
        (update (keyword result) merge
                {:amount (bigint (Integer/parseInt amount))})
        (update (keyword result) merge
                ingredient->amount))))

(defn format-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #" => "))
       (reduce build-reactions {})))

(defn without
  "Returns set s with x removed."
  [s x] (difference s #{x}))

(defn take-1
  "Returns the pair [element, s'] where s' is set s with element removed."
  [s] {:pre [(not (empty? s))]}
  (let [item (first s)]
    [item (without s item)]))

(defn no-incoming
  "Returns the set of nodes in graph g for which there are no incoming
  edges, where g is a map of nodes to sets of nodes."
  [g]
  (let [nodes         (set (keys g))
        have-incoming (apply union (set (vals g)))]
    (difference nodes have-incoming)))

(defn normalize
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (let [have-incoming (apply union (vals g))]
    (reduce #(if (get % %2) % (assoc % %2 #{})) g have-incoming)))

(defn kahn-sort
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, returns nil."
  ([g]
   (kahn-sort (normalize g) [] (no-incoming g)))
  ([g l s]
   (if (empty? s)
     (when (every? empty? (vals g)) l)
     (let [[n s'] (take-1 s)
           m      (g n)
           g'     (reduce #(update-in % [n] without %2) g m)]
       (recur g' (conj l n)
              (union s' (intersection (no-incoming g') m)))))))

(defn get-simple-formulas [reactions]
  (zipmap
    (keys reactions)
    (map (comp set #(remove #{:amount} %) keys)
         (vals reactions))))

(defn breakdown [reactions key required]
  (let [components  (-> reactions key)
        convertable (bigint (math/ceil (/ required (:amount components))))]
    (zipmap (keys (dissoc components :amount))
            (map (partial * convertable)
                 (vals (dissoc components :amount))))))


(defn part1 [input fuel]
  (let [reactions       (format-input input)
        sorted-keys     (kahn-sort (get-simple-formulas reactions))
        merge-with-plus (partial merge-with +)]
    (reduce (fn [acc key]
              (-> acc
                  (merge-with-plus
                    (breakdown reactions key (get acc key fuel)))
                  (dissoc key)))
            {} (remove #{:ORE} sorted-keys))))

(defn part2-binary-search [input]
  (let [one-fuel-ore  (:ORE (part1 puzzle-input 1))
        low-estimate  (int (math/floor (/ 1000000000000N one-fuel-ore)))
        high-estimate (int (math/floor (/ 1200000000000N one-fuel-ore)))]
    (loop [fuel      (+ low-estimate (int (/ (- high-estimate low-estimate) 2)))
           lowbound  low-estimate
           highbound high-estimate]
      (pm/dump :penguin (comp (map #(select-keys % [:fuel :runcount]))
                              (xf/take-last 10)))
      (let [required-ore (:ORE (part1 puzzle-input fuel))]
        (if (< (math/abs (- required-ore 1000000000000)) one-fuel-ore) fuel
            (recur (if (< required-ore 1000000000000)
                     (+ fuel (int (/ (- highbound fuel) 2)))
                     (- fuel (int (/ (- fuel lowbound) 2))))
                   (if (< required-ore 1000000000000) fuel lowbound)
                   (if (< required-ore 1000000000000) highbound fuel)))))))

(comment
  (part1 puzzle-input 1)
  ;; => {:ORE 168046N}

  ;; PART2 (Hand binary search)
  ;; 6972986

  (part2-binary-search puzzle-input)
  ;; => 6972986

  (pm/logs)

  (pm/reset!)

  0)

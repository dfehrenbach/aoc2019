(ns day6.core
  (:require [clojure.string :as str]))

(def problem-input (slurp "resources/day6/input.txt"))
(def test-input (slurp "resources/day6/test.txt"))


; Part 1


(defn split-input [input]
  (map #(str/split % #"\)")
       (str/split input #"\r\n")))

(defn build-map [acc [planet orbitted-by]]
  (let [orbitters (get-in acc [(keyword planet) :orbitters])]
    (assoc acc (keyword planet)
           {:orbitters (conj (or orbitters []) (keyword orbitted-by))
            :depth     0})))

(defn construct-map [input]
  (reduce build-map {} (split-input input)))

(defn travel-system-rec [!accmap current-planet current-depth from-planet]
  (if (nil? (get (deref !accmap) current-planet))
    (swap! !accmap #(assoc % current-planet
                           {:nearby-planets [from-planet]
                            :depth          current-depth}))

    (let [current-orbiters (get-in (deref !accmap) [current-planet :orbitters])]
      (doseq [new-planet current-orbiters]
        (swap! !accmap #(assoc % current-planet
                               {:depth          current-depth
                                :nearby-planets (conj current-orbiters from-planet)}))
        (travel-system-rec
         !accmap
         new-planet
         (inc current-depth)
         current-planet)))))

(defn travel-system [map]
  (let [!map (atom map)]
    (travel-system-rec !map :COM 0 nil)
    (deref !map)))

(defn sum-depths [input]
  (reduce + (map (fn [[_ {depth :depth}]] depth)
                 (travel-system (construct-map input)))))

(comment
  (sum-depths test-input)
 ;; => 42
 ;; => 54 (With YOU and SAN)
  
  (sum-depths problem-input)
  (travel-system (construct-map test-input)))
 ;; => 271151


; Part 2


(def !distance-to-santa (atom 0))

(defn draw-graph-rec [system visited current-planet distance]
  (println visited current-planet distance)
  (if (= current-planet :SAN)
    (throw (swap! !distance-to-santa (fn [_] (- distance 2))))
    (doseq [new-planet (remove visited (get-in system [current-planet :nearby-planets]))]
      (draw-graph-rec
       system
       (conj visited current-planet)
       new-planet
       (inc distance)))))

(defn draw-graph [system]
  (draw-graph-rec system #{} :YOU 0)) ;; Use #{set} to enable (remove visited ...) 


(comment
  (draw-graph (travel-system (construct-map test-input)))
  (deref !distance-to-santa)
  ;; => 4
  
  (draw-graph (travel-system (construct-map problem-input)))
  (deref !distance-to-santa))
  ;; => 388


; REHASH of Parts 1 and 2 (Alternate approach)

(defn format-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #"\)"))))

(defn orbiteer->orbited [orbit-pairs]
  (->> orbit-pairs
       (map (fn [[orbited orbiteer]] {orbiteer orbited}))
       (apply merge)))

(defn get-path-to-com [orbit-map current-planet]
  (when current-planet
    (cons current-planet
          (get-path-to-com orbit-map (orbit-map current-planet)))))

(defn sum-distances-from-com [input]
  (let [orbit-map (orbiteer->orbited (format-input input))]
    (->> (keys orbit-map)
         (map #(get-path-to-com orbit-map %))
         (map (comp dec count)) ;; Subtract target from length
         (reduce +))))

(defn distance-between-planets [input planet1 planet2]
  (let [orbit-map (orbiteer->orbited (format-input input))]
    (->> [planet1 planet2]
         (map #(get-path-to-com orbit-map %))
         (map set)
         (apply clojure.data/diff)
         (take 2) ;; clojure.data/diff gives [#{difference1} #{difference2} {similarities}]
         (map (comp dec count)) ;; Subtract target from length
         (reduce +))))

(comment
  (get-path-to-com (orbiteer->orbited (format-input (slurp "resources/day6/test.txt"))) "YOU")
  (sum-distances-from-com (slurp "resources/day6/test.txt"))
  ;; => 54
  
  (sum-distances-from-com (slurp "resources/day6/input.txt"))
  ;; => 271151
  
  (distance-between-planets (slurp "resources/day6/test.txt") "YOU" "SAN")
  ;; => 4
  
  (distance-between-planets (slurp "resources/day6/input.txt") "YOU" "SAN"))
  ;; => 388

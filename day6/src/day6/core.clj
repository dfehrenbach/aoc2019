(ns day6.core
  (:require [clojure.string :as str]))

(def problem-input (slurp "resources/input.txt"))
(def test-input (slurp "resources/test.txt"))


; Part 1


(defn split-input [input]
  (map #(str/split % #"\)")
       (str/split input #"\r\n")))

(defn build-map [acc [planet orbitted-by]]
  (let [orbitters (get-in acc [(keyword planet) :orbitters])]
    (assoc acc (keyword planet)
           {:orbitters (conj (or orbitters []) (keyword orbitted-by))
            :depth 0})))

(defn construct-map [input]
  (reduce build-map {} (split-input input)))

(defn travel-system-rec [!accmap current-planet current-depth from-planet]
  (if (nil? (get (deref !accmap) current-planet))
    (swap! !accmap #(assoc % current-planet
                           {:nearby-planets [from-planet]
                            :depth current-depth}))

    (let [current-orbiters (get-in (deref !accmap) [current-planet :orbitters])]
      (doseq [new-planet current-orbiters]
        (swap! !accmap #(assoc % current-planet
                               {:depth current-depth
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

(defn draw-map-rec [system visited current-planet distance]
  (println visited current-planet distance)
  (if (= current-planet :SAN)
    (throw (swap! !distance-to-santa (fn [_] (- distance 2))))
    (doseq [new-planet (remove visited (get-in system [current-planet :nearby-planets]))]
      (draw-map-rec
       system
       (conj visited current-planet)
       new-planet
       (inc distance)))))

(defn draw-map [system]
  (draw-map-rec system #{} :YOU 0)) ;; Use #{set} to enable (remove visited ...) 


(comment
  (draw-map (travel-system (construct-map test-input)))
  (deref !distance-to-santa)
  ;; => 4

  (draw-map (travel-system (construct-map problem-input)))
  (deref !distance-to-santa))
  ;; => 388

(ns day10.core)

(defn normalize-input [input] (clojure.string/split-lines input))

(defn build-coordinates [normalized-input] 
  (apply concat 
         (map-indexed (fn [y line] 
                        (filter some? 
                                (map-indexed (fn [x elem] 
                                               (when (= \# elem) [x y])) 
                                             line))) 
                      normalized-input)))

(defn slope [[x1 y1] [x2 y2]]
  (let [direction (cond (and (= x1 x2) (< y1 y2)) :vertically-up
                        (and (= x1 x2) (< y2 y1)) :vertically-down
                        (< x1 x2) :right
                        (< x2 x1) :left)
        delta-x   (- x2 x1)
        delta-y   (- y2 y1)]
    {:direction     direction
     :slope         (if (zero? delta-x) 0
                        (/ (- y2 y1) (- x2 x1)))
     :target-meteor [x2 y2]
     :distance      (Math/sqrt (+ (* delta-x delta-x) (* delta-y delta-y)))}))

(defn meteor-info [meteor all-meteors]
  (let [other-meteors           (remove #{meteor} all-meteors)
        other-meteor-info       (map #(slope meteor %) other-meteors)
        grouped-meteors         (group-by (fn [info] {:direction (:direction info)
                                                      :slope     (:slope info)}) other-meteor-info)
        visible-closest-meteors (map (fn [[_key grouping]] (first (sort-by :distance > grouping))) grouped-meteors)]
    {:meteor          meteor 
     :visible-meteors visible-closest-meteors}))

(defn day10-part1 [input]
  (let [coordinates     (->> input
                             normalize-input
                             build-coordinates)
        visible-meteors (->> coordinates 
                             (map (fn [meteor] (meteor-info meteor coordinates)))
                             (map (fn [info] (update info :visible-meteors count))))]
    (apply max-key :visible-meteors visible-meteors)))

(defn day10-part2 [input]
  (let [all-meteors     (->> input
                             normalize-input
                             build-coordinates)
        winning-meteor  (->> all-meteors 
                             (map (fn [meteor] (meteor-info meteor all-meteors)))
                             (apply max-key (comp count :visible-meteors)))
        grouped-meteors (group-by :direction (:visible-meteors winning-meteor))
        ordered-meteors (vec (concat (:vertically-up grouped-meteors)
                                     (sort-by :slope > (:right grouped-meteors))
                                     (:vertically-down grouped-meteors)
                                     (sort-by :slope < (:left grouped-meteors))))
        winning-meteor  (get ordered-meteors 200)]
    winning-meteor))

(comment 
  (day10-part1 (slurp "resources/test1.txt")) ;; Expected: :meteor [3 4] :visible-meteors 8
  ;; = {:meteor [3 4], :visible-meteors 8}
  
  (day10-part1 (slurp "resources/test2.txt")) ;; Expected: :meteor [5 8] :visible-meteors 33 
  ;; => {:meteor [5 8], :visible-meteors 33}
  
  (day10-part1 (slurp "resources/test3.txt")) ;; Expected: :meteor [1 2] :visible-meteors 35 
  ;; => {:meteor [1 2], :visible-meteors 35}
  
  (day10-part1 (slurp "resources/test4.txt")) ;; Expected: :meteor [6 3] :visible-meteors 41
  ;; => {:meteor [6 3], :visible-meteors 41}
  
  (day10-part1 (slurp "resources/test5.txt")) ;; Expected: :meteor [11 13] :visible-meteors 210
  ;; => {:meteor [11 13], :visible-meteors 210}
  
  (day10-part1 (slurp "resources/input.txt")) ;; PART 1
  ;; => {:meteor [11 13], :visible-meteors 227}
  
  (day10-part2 (slurp "resources/input.txt"))) ;; PART 2

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

(slope [11 13] [12 1])

(defn meteor-info [meteor all-meteors]
  (let [other-meteors           (remove #{meteor} all-meteors)
        other-meteor-info       (map #(slope meteor %) other-meteors)
        grouped-meteors         (group-by (fn [info] {:direction (:direction info)
                                                      :slope     (:slope info)}) other-meteor-info)
        visible-closest-meteors (map (fn [[_key grouping]] (first (sort-by :distance < grouping))) grouped-meteors)]
    {:meteor          meteor 
     :visible-meteors visible-closest-meteors}))

(meteor-info [1 1] [[1 3] [1 5] [1 2] [1 -1]])

(defn day10-part1 [input]
  (let [coordinates     (->> input
                             normalize-input
                             build-coordinates)
        visible-meteors (->> coordinates 
                             (map (fn [meteor] (meteor-info meteor coordinates)))
                             (map (fn [info] (update info :visible-meteors count))))]
    (apply max-key :visible-meteors visible-meteors)))

(defn day10-part2 [input num]
  (let [all-meteors     (->> input
                             normalize-input
                             build-coordinates)
        lazer-station   (->> all-meteors 
                             (map (fn [meteor] (meteor-info meteor all-meteors)))
                             (apply max-key (comp count :visible-meteors)))
        grouped-meteors (group-by :direction (:visible-meteors lazer-station))
        ordered-meteors (vec (concat (:vertically-up grouped-meteors)
                                     (sort-by :slope > (:right grouped-meteors))
                                     (:vertically-down grouped-meteors)
                                     (sort-by :slope < (:left grouped-meteors))))
        winning-meteor  (get ordered-meteors num)]
    winning-meteor))

(comment 
  (day10-part1 (slurp "resources/day10/test1.txt")) ;; Expected: :meteor [3 4] :visible-meteors 8
  ;; = {:meteor [3 4], :visible-meteors 8}
  
  (day10-part1 (slurp "resources/day10/test2.txt")) ;; Expected: :meteor [5 8] :visible-meteors 33 
  ;; => {:meteor [5 8], :visible-meteors 33}
  
  (day10-part1 (slurp "resources/day10/test3.txt")) ;; Expected: :meteor [1 2] :visible-meteors 35 
  ;; => {:meteor [1 2], :visible-meteors 35}
  
  (day10-part1 (slurp "resources/day10/test4.txt")) ;; Expected: :meteor [6 3] :visible-meteors 41
  ;; => {:meteor [6 3], :visible-meteors 41}
  
  (day10-part1 (slurp "resources/day10/test5.txt")) ;; Expected: :meteor [11 13] :visible-meteors 210
  ;; => {:meteor [11 13], :visible-meteors 210}
  
  (day10-part1 (slurp "resources/day10/input.txt")) ;; PART 1
  ;; => {:meteor [11 13], :visible-meteors 227}
  
  ;; Expected {:1   [11 12] :2   [12 1] :3   [12 2]  :10  [12 8] 
  ;;           :20  [16 0]  :50  [16 9] :100 [10 16] :199 [9 6] 
  ;;           :200 [8 2]   :201 [10 9] :299 [11 1]}
  ;; NOTE: These have the right side meteors REVERSED and starting at the 
  ;;       BOTTOM rather than the TOP
  ;;        THE EXPECTED IS WRONG
  ;; Note2: Also... adjust for 1th instead of 0th index
  (let [input (slurp "resources/day10/test5.txt")]
    (map (fn [num] {(inc num) (day10-part2 input num)}) 
         (map dec [1 2 3 10 20 50 100 199 200 201 299])))

  ;; Adjust for index starting at 1 (No such thing as the 0th meteor to be vaporized)
  (day10-part2 (slurp "resources/day10/input.txt") (dec 200)) ;; PART 2
  ;; => {:direction :left, :slope 9/5, :target-meteor [6 4], :distance 10.295630140987}
  ;; ANSWER: 640
  )

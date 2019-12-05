(ns day4.core)

(def low 382345)
(def high 843167)


; Part 1


(defn passwords-in-range [low high]
  (let [all-passwords (for [a (range 1 10)
                            b (range a 10)
                            c (range b 10)
                            d (range c 10)
                            e (range d 10)
                            f (range e 10)]
                        (+ (* a 100000) (* b 10000) (* c 1000) (* d 100) (* e 10) (* f 1)))]
    (filter #(and (< low %) (< % high)) all-passwords)))

(map clojure.string/join (partition 2 1 "123456"))

(defn has-adjacent-numerals [number]
  (let [str-num (str number)
        pairs (map clojure.string/join (partition 2 1 str-num))]
    (some true? (map #(= (first %) (second %)) pairs))))

(defn solve-part1 [low high]
  (filter has-adjacent-numerals (passwords-in-range low high)))

(comment

  (count (solve-part1 low high)))
  ;; => 460


; Part 2


(defn has-at-least-one-pair-of-two [number]
  (let [str-num (str number)
        digitcounts (map #(count (val %)) (group-by identity str-num))]
    (some #(= % 2) digitcounts)))

(defn solve-part2 [low high]
  (filter has-at-least-one-pair-of-two (passwords-in-range low high)))

(comment

  (count (solve-part2 low high)))
  ;; => 290

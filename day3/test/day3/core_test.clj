(ns day3.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [day3.core :refer [create-points
                               build-lines
                               manhatten-distance
                               part1-solution
                               travelled-distance]]))

(deftest create-points-test
  (testing "Handles up"
    (is (= [[0 0] [0 5]]
           (create-points ["U5"]))))
  (testing "Handles down"
    (is (= [[0 0] [0 -6]]
           (create-points ["D6"]))))
  (testing "Handles right"
    (is (= [[0 0] [8 0]]
           (create-points ["R8"]))))
  (testing "Handles left"
    (is (= [[0 0] [-7 0]]
           (create-points ["L7"]))))
  (testing "Handles two different axis"
    (is (= [[0 0] [0 5] [-7 5]]
           (create-points ["U5" "L7"]))))
  (testing "Handles two same axis"
    (is (= [[0 0] [0 5] [0 -1]]
           (create-points ["U5" "D6"])))))

(deftest build-lines-test
  (testing "Handles horizontal lines"
    (is (= [[1 0] [2 0] [3 0] [4 0] [5 0]]
           (build-lines [[0 0] [5 0]]))))
  (testing "Handles vertical lines"
    (is (= [[0 1] [0 2] [0 3] [0 4] [0 5]]
           (build-lines [[0 0] [0 5]]))))
  (testing "Handles 3 points forming a corner"
    (is (= [[1 0] [2 0] [2 1] [2 2] [2 3]]
           (build-lines [[0 0] [2 0] [2 3]]))))
  (testing "Handles 3 points forming a line"
    (is (= [[1 0] [2 0] [3 0] [4 0] [5 0]]
           (build-lines [[0 0] [2 0] [5 0]])))))

(deftest manhatten-distance-test
  (testing "Given a number with 0, gives the number back"
    (is (= 5 (manhatten-distance [0 5]))))
  (testing "Given two positive numbers, returns them added together"
    (is (= 10 (manhatten-distance [5 5]))))
  (testing "Given a positive and negative together, uses absolute value"
    (is (= 10 (manhatten-distance [-5 5])))))

(deftest part1-solution-test
  (testing "4 Point lines example"
    (is (= 6 (:distance (part1-solution [["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"]])))))
  (testing "4 Point lines example"
    (is (= 159 (:distance (part1-solution [["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
                                           ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])))))
  (testing "4 Point lines example"
    (is (= 135 (:distance (part1-solution [["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
                                           ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"]]))))))

(deftest travelled-distance-test
  (testing "Given a set of crosses and the original lines, give a map of distances travelled for each wire"
    (is (= [{:l1 10 :l2 10 :cross [5 5]}] (travelled-distance [[5 5]] [["R5" "U5"] ["U5" "R5"]])))))

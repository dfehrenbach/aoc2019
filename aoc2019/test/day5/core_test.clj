(ns day5.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [day5.core :refer [day5-build-program
                               final-output
                               program-input]]))

(deftest build-program-test
  (testing "Given 1 0 0 0 99, replace the 1 with a 2"
    (is (= [2 0 0 0 99]
           (day5-build-program [1 0 0 0 99]))))
  (testing "Given 2 3 0 3 99, replace the second 3 with a 6"
    (is (= [2 3 0 6 99]
           (day5-build-program [2 3 0 3 99]))))
  (testing "Given 2 4 4 5 99 0, replace 0 with 9801, a number beyond 99"
    (is (= [2 4 4 5 99 9801]
           (day5-build-program [2 4 4 5 99 0]))))
  (testing "Given 1 1 1 4 99 5 6 0 99, replace multiple numbers"
    (is (= [30 1 1 4 2 5 6 0 99]
           (day5-build-program [1 1 1 4 99 5 6 0 99])))))

(deftest build-program-day5-part1
  (testing "Given 3 0 4 0 99, places some input, reads it, and then stops"
    (swap! program-input (fn [_] 1))
    (is (= [1 0 4 0 99]
           (day5-build-program [3 0 4 0 99]))))
  (testing "Opcodes with input option 1 use immediate mode"
    (is (= [9 1 9 0 99]
           (day5-build-program [1002 1 9 0 99])))))

(deftest build-program-day5-part2
  (testing "If the input is 8 will output 1, 0 otherwise. Position Mode"
    (let [test-program [3 9 8 9 10 9 4 9 99 -1 8]]
      (swap! program-input (fn [_] 8))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] 7))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 9))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))))

  (testing "If the input is less than 8 will output 1, 0 otherwise. Position Mode"
    (let [test-program [3 9 7 9 10 9 4 9 99 -1 8]]
      (swap! program-input (fn [_] 7))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] 8))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 9))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))))

  (testing "If the input is 8 will output 1, 0 otherwise. Immediate Mode"
    (let [test-program [3 3 1108 -1 8 3 4 3 99]]
      (swap! program-input (fn [_] 8))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] 7))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 9))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))))

  (testing "If the input is less than 8 will output 1, 0 otherwise. Immediate Mode"
    (let [test-program [3 3 1107 -1 8 3 4 3 99]]
      (swap! program-input (fn [_] 7))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] 8))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 9))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))))

  (testing "If the input is 0 then will output 0, 1 otherwise. Position Mode"
    (let [test-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]]
      (swap! program-input (fn [_] 0))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 1))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] -1))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))))

  (testing "If the input is 0 then will output 0, 1 otherwise. Immediate Mode"
    (let [test-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]]
      (swap! program-input (fn [_] 0))
      (day5-build-program test-program)
      (is (= 0 (deref final-output)))

      (swap! program-input (fn [_] 1))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))

      (swap! program-input (fn [_] -1))
      (day5-build-program test-program)
      (is (= 1 (deref final-output)))))

  (testing "If the input is below 8 it will output 999, 8 -> 1000, and above 8 -> 1001"
    (let [test-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                        1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                        999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
      (swap! program-input (fn [_] 7))
      (day5-build-program test-program)
      (is (= 999 (deref final-output)))

      (swap! program-input (fn [_] 8))
      (day5-build-program test-program)
      (is (= 1000 (deref final-output)))

      (swap! program-input (fn [_] 9))
      (day5-build-program test-program)
      (is (= 1001 (deref final-output))))))

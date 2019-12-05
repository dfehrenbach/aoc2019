(ns day2.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [day2.core :refer [build-program]]))

(deftest build-program-test
  (testing "Given 1 0 0 0 99, replace the 1 with a 2"
    (is (= [2 0 0 0 99]
           (build-program [1 0 0 0 99]))))
  (testing "Given 2 3 0 3 99, replace the second 3 with a 6"
    (is (= [2 3 0 6 99]
           (build-program [2 3 0 3 99]))))
  (testing "Given 2 4 4 5 99 0, replace 0 with 9801, a number beyond 99"
    (is (= [2 4 4 5 99 9801]
           (build-program [2 4 4 5 99 0]))))
  (testing "Given 1 1 1 4 99 5 6 0 99, replace multiple numbers"
    (is (= [30 1 1 4 2 5 6 0 99]
           (build-program [1 1 1 4 99 5 6 0 99])))))

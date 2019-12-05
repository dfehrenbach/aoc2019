(ns day1.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [day1.core :refer [calculate-fuel
                               solve-modules-weight
                               calculate-recursive-fuel]]))

(deftest calculate-fuel-test
  (testing "For a mass of 12, the fuel required is 2"
    (is (= 2 (calculate-fuel 12))))
  (testing "For a mass of 14 the fuel required is also 2"
    (is (= 2 (calculate-fuel 14))))
  (testing "For a mass of 1969, the fuel required is 654"
    (is (= 654 (calculate-fuel 1969))))
  (testing "For a mass of 100756, the fuel required is 33583"
    (is (= 33583 (calculate-fuel 100756)))))

(deftest solve-modules-test
  (testing "For a mass of 12, the fuel required is 2"
    (is (= 34241 (solve-modules-weight [12 14 1969 100756])))))

(deftest calculate-recursive-fuel-test
  (testing "A module of mass 14 requires only 2 fuel"
    (is (= 2 (calculate-recursive-fuel 14))))
  (testing "A module of mass 1969 requires 654 originally, but eventually 966 fuel"
    (is (= 966 (calculate-recursive-fuel 1969))))
  (testing "A module of mass 100756 requires a total of 50346 fuel"
    (is (= 50346 (calculate-recursive-fuel 100756)))))

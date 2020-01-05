(ns day14.core-test
  (:require [day14.core :as sut]
            [clojure.test :as t]))


(t/deftest part1
  (t/testing "test1"
    (t/is (= {:ORE 31}
             (sut/part1 (slurp "resources/day14/test1.txt")))))
  (t/testing "test2"
    (t/is (= {:ORE 165}
             (sut/part1 (slurp "resources/day14/test2.txt")))))
  (t/testing "test3"
    (t/is (= {:ORE 13312}
             (sut/part1 (slurp "resources/day14/test3.txt")))))
  (t/testing "test4"
    (t/is (= {:ORE 180697}
             (sut/part1 (slurp "resources/day14/test4.txt")))))
  (t/testing "test5"
    (t/is (= {:ORE 2210736}
             (sut/part1 (slurp "resources/day14/test5.txt")))))
  )

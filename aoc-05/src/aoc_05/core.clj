(ns aoc-05.core
  (:gen-class)
  (:require [clojure.string :as str]
            [aoc-2019.intcode.v2 :refer :all]))


(def test1 [3 0 4 0 99])
(def test2 [1002 4 3 4 33])

(def test3 [3 9 8 9 10 9 4 9 99 -1 8])
(def test4 [3 9 7 9 10 9 4 9 99 -1 8])
(def test5 [3 3 1108 -1 8 3 4 3 99])
(def test6 [3 3 1107 -1 8 3 4 3 99])

(def test7 [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9])
(def test8 [3 3 1105 -1 9 1101 0 0 12 4 12 99 1])

(def test9 [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
            1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
            999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99])

(def test-file "aoc-05.txt")

(defn run-test [test]
  (reset-vm)
  (load-program test)
  (execute))

(def run-test-file (comp run-test read-csv-numbers))

(defn -main
  [& args]
  (run-test-file test-file))

(ns aoc-2019.aoc-04
  (:require [clojure.string :as str]))

(def raw-input "134564-585159")
(def input
  (let [strs (str/split raw-input #"-")
        nums (map #(Long/parseLong %) strs)]
    (map str
         (range (first nums) (inc (second nums))))))

;; Predicates

(defn six-digit [x]
  (= 6 (count x)))

(defn adj-pairs [x]
  (partition 2 1 x))

(defn some-pair-eql [pairs]
  (some (partial apply =) pairs))

(defn two-adj-digits [x]
  (some-pair-eql (adj-pairs x)))

(defn non-decreasing-digits [x]
  (every? #(or (neg? %) (zero? %))
          (map (partial apply compare) (adj-pairs x))))

(defn password-predicate [x]
  (and (six-digit x)
       (two-adj-digits x)
       (non-decreasing-digits x)))


(defn count-occurs
  "Count occurrences of x in s"
  [s x]
  (count (filter #{x} s)))

(defn contains-strict-double [x]
  (some #(= 2 %)
        (map (partial count-occurs x) (set x))))

(defn password-predicate2 [x]
  (and (six-digit x)
       (contains-strict-double x)
       (non-decreasing-digits x)))

;; Driver

(defn test-values [pred]
  (->> input
       (pmap pred)
       (filter identity)
       (count)))

(defn test1 []
  (test-values password-predicate))

(defn test2 []
  (test-values password-predicate2))

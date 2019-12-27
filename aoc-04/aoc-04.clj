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

(defn two-adj-digits [x]
  (some (partial apply =) (adj-pairs x)))

(defn non-decreasing-digits [x]
  (every? #(or (neg? %) (zero? %))
          (map (partial apply compare) (adj-pairs x))))

(defn password-predicate [x]
  (and (six-digit x)
       (two-adj-digits x)
       (non-decreasing-digits x)))

;; Driver

(defn test-values []
  (->> input
       (pmap password-predicate)
       (filter identity)
       (count)))

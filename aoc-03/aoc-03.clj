(ns aoc-2019.aoc-03
  (:require
   [clojure.string :as str]
   [clojure.set :refer [intersection]]))

(def inputfile "aoc-03.txt")
(def examples
  [{:wire ["R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"]
    :distance 159}
   {:wire ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    :distance 135}])

(defn move [pos instr]
  (let [[x y] pos
        dir (first instr)
        dist (Long/parseLong (str/join (rest instr)))]
    (case dir
      \L [(- x dist) y]
      \R [(+ x dist) y]
      \U [x (+ y dist)]
      \D [x (- y dist)])))

(defn expand-move [begin end]
  (letfn [(birange [a b]
            (if (< a b) (range a b) (range a b -1)))]
    (let [[bx by] begin
          [ex ey] end]
      (if (= bx ex)
        (for [y (birange by ey)] [bx y])
        (for [x (birange bx ex)] [x by])))))

(defn turtle
  "Simulate moving a turtle on grid according to list of moves."
  [moves]
  (reductions move [0 0] moves))

(defn expand-turtle
  "Produces full path of turtle moves."
  [moves]
  (let [path (turtle moves)]
    (mapcat expand-move path (rest path))))

(defn manhattan-dist [a b]
  (let [[x1 y1] a
        [x2 y2] b]
    (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn path-dist
  "Number of path steps between points on the path"
  [a b path]
  (let [pa (.indexOf path a)
        pb (.indexOf path b)]
    (Math/abs (- pa pb))))

(defn total-path-dist
  [a b paths]
  (reduce +
          (map (partial path-dist a b) paths)))

(defn find-path-crossing [instrs]
  (let [paths (map expand-turtle instrs)
        sets (map (comp set rest) paths)]
    [(apply intersection sets)
     paths]))

(defn nearest-neighbour-distance [pt others]
  (apply min (map (partial manhattan-dist pt) others)))

(defn find-nearest-crossing-distance [instrs]
  (nearest-neighbour-distance
   [0 0]
   (first (find-path-crossing instrs))))

(defn nearest-path-distance [pt others paths]
  (apply min (map #(total-path-dist pt % paths) others)))

(defn find-nearest-crossing-path-distance [instrs]
  (let [[crossings paths] (find-path-crossing instrs)]
    (nearest-path-distance [0 0] crossings paths)))

;;; Interface

(defn read-csv-line
  [line]
  (as-> line l
    (str/split l #",")
    (map #(str/trim %) l)))

(defn read-csv
  "Lines in form x,y,z"
  [filename]
  (->> filename
    slurp
    str/split-lines
    (map read-csv-line)))

(defn run-test [instrs expected]
  (let [result (find-nearest-crossing-distance instrs)]
    (= result expected)))

(defn run-tests []
  (map
   (fn [{wires :wire distance :distance}]
     (run-test (map read-csv-line wires) distance))
   examples))

(defn run-file [filename]
  (let [instrs (read-csv filename)]
    (find-nearest-crossing-distance instrs)))

(defn run-file-path-dist [filename]
  (let [instrs (read-csv filename)]
    (find-nearest-crossing-path-distance instrs)))

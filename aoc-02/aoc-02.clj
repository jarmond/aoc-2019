(ns aoc-2019.aoc-02
  (:require [clojure.string :as str]))

(def inputfile "aoc-02.txt")

;;; Intcode VM
(def ops
  {1 +
   2 *})
(def op-len 4)

(defn func-name [f]
  (-> f meta :name))

(def halt 99)

(def memory (atom {}))

(defn reset-mem []
  (swap! memory (constantly {})))

(defn read-mem [loc]
  (get @memory loc 0))

(defn write-mem [loc val]
  (swap! memory assoc loc val))

(defn execute-static
  "Execute code as static program with separate memory"
  [code]
  (loop [[op & rest] code]
    (when-not (or (nil? op) (= op halt))
      (let [[locx locy locr & more] rest
            x (read-mem locx)
            y (read-mem locy)]
        (do
          (print "OP " op x y rest more))
          (newline)
          (write-mem locr ((ops op) x y))
          (recur more)))))

(defn execute-dynamic
  "Execute code as dynamic program tape which can self modify"
  [init len]
  (loop [pc init]
    (when (< pc len)
      (let [op (read-mem pc)
            ptrx (read-mem (+ pc 1))
            ptry (read-mem (+ pc 2))
            ptrr (read-mem (+ pc 3))]
        (when-not (or (zero? op) (= op halt))
          (let [x (read-mem ptrx)
                y (read-mem ptry)]
            (print "PC " pc "OP " op x y)
            (newline)
            (write-mem ptrr ((ops op) x y))
            (recur (+ pc 4))))))))

;;; Interface

(defn read-csv-numbers
  "Number line in form 1,2,3,4"
  [filename]
  (as-> filename f
       (slurp f)
       (str/split f #",")
       (mapv #(Long/parseLong (str/trim %)) f)))

(defn load-program [program]
  (dotimes [pc (count program)]
    (write-mem pc (program pc))))

(defn init-system []
  (write-mem 1 12)
  (write-mem 2 2))

(defn main []
  (let [program (read-csv-numbers inputfile)]
    (reset-mem)
    (load-program program)
    (init-system)
    (execute-dynamic 0 (count program))
    (print "POS 0 =" (read-mem 0))))

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

(def ^:dynamic *verbose* true)

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
          (when *verbose*
            (println "OP " op x y rest more))
          (write-mem locr ((ops op) x y))
          (recur more))))))

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
            (when *verbose*
              (println "PC " pc "OP " op x y))
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

(defn init-system [noun verb]
  (write-mem 1 noun)
  (write-mem 2 verb))

(defn run
  ([]
   (run 12 2))
  ([noun verb]
   (when *verbose*
     (println "N" noun "V" verb))
   (let [program (read-csv-numbers inputfile)]
     (reset-mem)
     (load-program program)
     (init-system noun verb)
     (execute-dynamic 0 (count program))
     (let [result (read-mem 0)]
       (when *verbose*
         (println "POS 0 =" result))
       result))))


(defn find-result [target]
  (binding [*verbose* false]
    (let [noun-verb (for [n (range 99) v (range 99)] [n v])]
      (take 1
            (drop-while #(not= (first %) target)
                        (map (bound-fn* (fn [[n v]] [(run n v) n v])) noun-verb))))))


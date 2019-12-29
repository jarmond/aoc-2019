(ns aoc-2019.intcode.v2
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

;;;; Intcode VM

(def ^:dynamic *verbose* true)

;;; State

(defrecord IntcodeMachine [memory pc])
(def vm (ref (map->IntcodeMachine {:memory [] :pc 0 :running? false})))

(defn reset-vm
  []
  (dosync
   (commute vm assoc :memory [])
   (commute vm assoc :pc 0)))

(defn read-mem
  [loc]
  {:pre (< loc (count (:memory @vm)))}
  (get-in @vm [:memory loc] 0))

(defn write-mem
  [loc val]
  {:pre (< loc (count (:memory @vm)))}
  (dosync
   (commute vm assoc-in [:memory loc] val)))

(defn top-mem []
  (count (:memory @vm)))

(defn load-program
  [program]
  (dosync
   (dotimes [i (count program)]
     (alter vm update :memory #(conj % (program i)))))
  (when *verbose*
    (println "LOADED" (count program))))

(defn set-pc
  [x]
  (dosync
   (commute vm assoc :pc x)))

(defn inc-pc
  [n]
  (dosync
   (commute vm update :pc (partial + n))))

(defn pc []
  (get @vm :pc))

(defn is-running []
  (get @vm :running?))

(defn toggle-running []
  (dosync
   (commute vm update :running? not)))

;;; Instructions

(def ops {})
(defmacro definstr
  "Define instruction for `opcode` accepting `args` and executes `body`,
  optionally returning a parameter to location `ret`, and incrementing the
  program counter unless set explicitly. The generated function takes one
  argument, `pos-modes`, a set denoting the positional arguments which should be
  treated as references. Note last argument is always treated as address (i.e.
  not deref'd)."
  [instr-name opcode args ret & body]
  (let [args-and-ret (if ret (conj args ret) args)
        nargs-and-ret (count args-and-ret)
        nargs (count args)
        sets-pc? ((-> body flatten set) 'set-pc)

        pos-modes (gensym)
        cur-pc (gensym)]

    (letfn [(get-arg [i]
              `(read-mem (+ ~cur-pc ~(inc i))))

            (when-pos-deref [arg pos]
              `(if (~pos-modes ~pos) (read-mem ~arg) ~arg))]

      `(do
         ;; Define instruction function.
         (defn ~instr-name [~pos-modes]
           (let [~cur-pc (pc)
                 ~@(interleave args-and-ret (map get-arg (range nargs-and-ret)))
                 ~@(interleave args (map when-pos-deref args (range nargs)))]
             (when (= *verbose* :debug)
               (cl-format true "  ~a ~{~a~^, ~}~@[ ret ~a~]~%" (name '~instr-name) ~args ~ret))
             ~@body)

           ;; Auto-increment pc if not set explicitly by function.
           ~(when-not sets-pc?
              `(inc-pc ~(inc nargs-and-ret))))

         ;; Associate instruction function with opcode.
         (alter-var-root #'ops assoc ~opcode
                         {:fn #'~instr-name :name (name '~instr-name)})))))

(definstr add 1 [x y] addr
  (write-mem addr (+ x y)))

(definstr mul 2 [x y] addr
  (write-mem addr (* x y)))

(definstr in 3 [] addr
  (print "INPUT? ")
  (->> (read-line)
       Integer/parseInt
       (write-mem addr))
  (newline))

(definstr out 4 [val] nil
  (println "OUT" val))

(definstr jump-if-true 5 [t addr] nil
  (if-not (zero? t)
    (set-pc addr)
    (inc-pc 3)))

(definstr jump-if-false 6 [t addr] nil
  (if (zero? t)
    (set-pc addr)
    (inc-pc 3)))

(definstr less-than 7 [x y] addr
  (write-mem addr (if (< x y) 1 0)))

(definstr equals 8 [x y] addr
  (write-mem addr (if (= x y) 1 0)))

(definstr halt 99 [] nil
  (toggle-running))

(defn extract-digit
  "Extract digit `i` from integer `x`. Indexed from zero, right-to-left."
  [x i]
  (let [divisor (Math/pow 10 i)]
    (-> x (/ divisor) (mod 10) int)))

(defn decode-opcode
  "Decode instruction in format ABCDE, where DE is opcode."
  [instr]
  (let [digit (partial extract-digit instr)]
    (+ (digit 0) (* 10 (digit 1)))))

(defn decode-modes
  "Decode instruction in format ABCDE, where A, B, and C are the modes of each
  parameter, leading zeros assumed."
  [instr]
  (let [digit (partial extract-digit instr)]
    (map digit (range 2 5))))

(defn indexed
  "Generate pairs of elements and their index."
  [coll]
  (map vector coll (range (count coll))))

(defn index-of-all
  "Find indexes of all occurrences of `x` in `coll`."
  [coll x]
  (->> coll
       indexed
       (filter #(= x (first %)))
       (map second)))

(defn execute
  "Execute code as dynamic program tape which can self modify"
  []
  (when-not (is-running)
    (toggle-running))
  (loop []
    (let [instr (read-mem (pc))
          op (decode-opcode instr)
          modes (decode-modes instr)
          {opfn :fn opname :name} (ops op)]
      (if (nil? opfn)
        (cl-format true "PC ~a ERR NIL" (pc))
        (do
          (when *verbose*
            (cl-format true "PC ~a OP ~a (~a) MODE ~a~%" (pc) op opname modes))

          ;; Execute
          (opfn (set (index-of-all modes 0)))

          (when (is-running)
            (recur)))))))

(defn init-system [noun verb]
  (write-mem 1 noun)
  (write-mem 2 verb))

(defn run-program
  ([noun verb]
   (when *verbose*
     (println "N" noun "V" verb) )
   (init-system noun verb)
   (run-program))
  ([]
   (execute)
   (let [result (read-mem 0)]
     (when *verbose*
       (println "POS 0 =" result))
     result)))

(defn read-csv-numbers
  "Number line in form 1,2,3,4"
  [filename]
  (as-> filename f
    (slurp f)
    (str/split f #",")
    (mapv #(Long/parseLong (str/trim %)) f)))

(defn run-file
  ([inputfile noun verb]
   (let [program (read-csv-numbers inputfile)]
     (reset-vm)
     (load-program program)
     (run-program noun verb))))


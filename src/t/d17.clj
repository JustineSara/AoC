(ns t.d17
  (:gen-class)
  (:require
    [clojure.math :as math]
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")
(def sol "4,6,3,5,6,3,5,2,1,0")

(def sample2
"Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")
(def sol2 117440)

(defn parse-input
  [input]
  (let [ [_ A B C instr] (re-find #"Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ([\d,?]+)" input)
        instr (->> (cljstr/split instr #",")
                   (map parse-long)
                   vec
                   )]
    [(parse-long A) (parse-long B) (parse-long C) instr]))


(defn combo-oprand
  [A B C oprand]
  (cond
    (<= oprand 3) oprand
    (= 4 oprand) A
    (= 5 oprand) B
    (= 6 oprand) C
    "7 or none of the above is pbm" (prn "Problem with oprand" oprand)))

(defn adv
  [A B C oprand p]
  [(quot A (int (math/pow 2 (combo-oprand A B C oprand))))
   B
   C
   nil
   (+ 2 p)])

(defn bxl
  [A B C oprand p]
  [A
   (bit-xor B oprand)
   C
   nil
   (+ 2 p)])

(defn bst
  [A B C oprand p]
  [A
   (mod (combo-oprand A B C oprand) 8)
   C
   nil
   (+ 2 p)])

(defn jnz
  [A B C oprand p]
  [A
   B
   C
   nil
   (if (zero? A) (+ 2 p) oprand)])

(defn bxc
  [A B C _ p]
  [A
   (bit-xor B C)
   C
   nil
   (+ 2 p)])

(defn out
  [A B C oprand p]
  [A
   B
   C
   (mod (combo-oprand A B C oprand) 8)
   (+ 2 p)])

(defn bdv
  [A B C oprand p]
  [A
   (quot A (int (math/pow 2 (combo-oprand A B C oprand))))
   C
   nil
   (+ 2 p)])

(defn cdv
  [A B C oprand p]
  [A
   B
   (quot A (int (math/pow 2 (combo-oprand A B C oprand))))
   nil
   (+ 2 p)])

(def opcode-to-fn
  {0 adv
   1 bxl
   2 bst
   3 jnz
   4 bxc
   5 out
   6 bdv
   7 cdv })

(defn d17p1
  [input]
  (let [[A B C instr] (parse-input input) ]
    (loop [A A
           B B
           C C
           p 0
           outs []]
      (let [opcode (get instr p)
            oprand (get instr (inc p))]
        (if (nil? opcode)
          (->> outs
               (map (comp str int))
               (interpose ",")
               (apply str))
          (let [[A B C out p] ((get opcode-to-fn opcode) A B C oprand p)
;;                _ (prn [:operation opcode oprand])
;;                _ (prn [A B C out p])
                outs (if out (conj outs out) outs)]
            (recur A B C p outs)))))))

(defn one-series
  [A]
  (let [B (mod A 8)
        C (quot A (int (math/pow 2 (* 2 B))))
        A (quot A (int (math/pow 2 (+ 3 B))))
        B (bit-xor B 6)
        B (bit-xor B C)]
    [A out]
    ))

(defn d17p2-brute
  [input]
  (let [[A B-i C-i instr] (parse-input input) ]
    (loop [Astart 77500
           A 0
           B B-i
           C C-i
           p 0
           outs []]
      (let [opcode (get instr p)
            oprand (get instr (inc p))]
        (if (nil? opcode)
          (if (= outs instr)
            Astart
            (do (when (zero? (rem Astart 1000)) (prn [Astart outs instr]))
                (recur (inc Astart) (inc Astart) B-i C-i 0 [])))
          (let [[A B C out p] ((get opcode-to-fn opcode) A B C oprand p)
                outs (if out (conj outs out) outs)]
            (comment
              (prn [:start Astart])
              (prn [:operation opcode oprand])
              (prn [A B C out p])
              (prn [:instr instr :outs outs])
              )
            (if (every? true? (map = outs instr))
              (recur Astart A B C p outs)
              (do (when (zero? (rem Astart 1000)) (prn [Astart outs instr]))
                  (recur (inc Astart) (inc Astart) B-i C-i 0 [])))))))))


(defn all-Astart
  [Aend out]
  (for [B1 (range 8)
        :let [B2 (bit-xor B1 5)
              B3 (bit-xor B2 6)
              As (+ B1 (* 8 Aend))
              C (quot As (int (math/pow 2 B2)))
              B4 (bit-xor C B3)]
        :when (= out (mod B4 8))]
    As
    ))
(comment
(all-Astart 0 0)
(all-Astart 3 3)
(all-Astart 192 6)
)

(defn d17p2
  [input]
  (let [[_ _ _ instr] (parse-input input)]
    (loop [list-options[[0 (reverse instr)]]
           ]
      (prn [:list list-options])
      (if (empty? list-options) "Problem, no more options!"
        (let [[[Aend outputs] & list-options] list-options]
          (if (empty? outputs) Aend
            (let [[out & rest-out] outputs
                  Astart-s (all-Astart Aend out)]
              (prn [:Aend Aend :out out :Astart-s Astart-s])
              (if (empty? Astart-s)
                (recur list-options)
                (recur (sort-by first (concat list-options (map (fn [a] [a rest-out]) Astart-s))))
                ))))))))


(defn debug-part-2
  [input]
  (let [[_ _ _ instr] (parse-input input) ]
    (loop [A 12312
           B 0
           C 0
           p 0
           outs []]
      (let [opcode (get instr p)
            oprand (get instr (inc p))]
        (if (nil? opcode)
          (->> outs
               (map (comp str int))
               (interpose ",")
               (apply str))
          (let [[A B C out p] ((get opcode-to-fn opcode) A B C oprand p)
;;                _ (prn [:operation opcode oprand])
                _ (prn [A B C out p])
                outs (if out (conj outs out) outs)]
            (recur A B C p outs)))))))


(defn -main
  [& args]
  (println "day17")
  (println sample)
  (newline)

  (comment
  (println "part1")
  (prn (d17p1 sample))
  (prn sol)
  (prn (d17p1 (slurp "input/day17.txt")))
  )

  (newline)
  (println "part2")
  (prn (d17p2 sample2))
  (prn sol2)
  (prn (d17p2 (slurp "input/day17.txt")))
  (comment
  (prn (debug-part-2 (slurp "input/day17.txt")))
  )
  )


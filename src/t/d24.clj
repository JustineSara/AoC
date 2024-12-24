(ns t.d24
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample1
"x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")
(def sol1 4)

(def sample2
"x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")
(def sol2 2024)


(defn parse-input
  [input]
  (let [[w g] (cljstr/split input #"\n\n")
        w (->> w
            (cljstr/split-lines)
            (map (fn [l] (cljstr/split l #": ")))
            (map (fn [[w v]] {w (= v "1")}))
            (into {})
            )
        g (->> (re-seq #"(\S{3}) ((AND)|(OR)|(XOR)) (\S{3}) -> (\S{3})" g)
               (map (fn[[_ w1 op _ _ _ w2 wo]] [w1 w2 op wo]))
               )]
[w g]
  ))

(defn OP
  [op w1 w2]
  (case op
    "AND" (and w1 w2)
    "OR" (or w1 w2)
    "XOR" (not= w1 w2)))

(defn d24p1
  [input]
  (let [[kn-wi gates] (parse-input input)
        all-wi (loop [wi kn-wi
                      gs (vec gates)]
                 (if (empty? gs) wi
                   (let [[[w1 w2 op wo :as g] & gs] gs
                         v1 (wi w1)
                         v2 (wi w2)]
                     (if (or (nil? v1) (nil? v2))
                       (recur wi (concat gs [g]))
                       (recur (assoc wi wo (OP op v1 v2)) gs)))))]
    (->> all-wi
         (keep (fn [[k v]] (when (= (first k) \z) [k v])))
         (sort-by first)
         (map second)
         (map (fn [v] (if v 1 0)))
         (reverse)
         (reduce (fn [c n] (+ n  (* 2 c))) 0)

    )))

(defn sum
  [a b r]
  (case (+ (if a 1 0) (if b 1 0) (if r 1 0))
    0 [false false]
    1 [true false]
    2 [false true]
    3 [true true]
    ))

(defn get-gates
  [z gates]
  (loop [kgates []
         ws #{z}]
    (let [nkg (keep (fn [[_ _ _ wo :as g]] (when (contains? ws wo) g) ) gates)
          nws (apply conj ws (mapcat (fn [[w1 w2 _ wo]] [w1 w2 wo]) nkg))]
      (if (= (count kgates) (count nkg)) nkg
        (recur nkg nws))
    )))

(defn build-xy
  [x y r i]
  (if (zero? i) {"x00" x "y00" y}
    (merge {(format "x%02d" i) x (format "y%02d" i) y (format "y%02d" (dec i)) r (format "x%02d" (dec i)) r}
           (apply merge
                  (for [ii (range (dec i))]
                    {(format "x%02d" ii) false (format "y%02d" ii) false}))
           )))
(comment
  (build-xy true true true 0)
  (build-xy true true true 1)
  (build-xy true false true 2)
  )

(defn d24p2
  [input]
  (let [[_ gates] (parse-input input)
        ]

    (comment
      (prn (get-gates "z01" gates))
      (prn (get-gates "z02" gates))
      )
    (loop [trust-g []
           un-g (set gates)
           i 0]
      (if (= i 46) (prn "The End")
      (let [z (format "z%02d" i)
            test-g (get-gates z un-g)
            _ (prn [:iter i :z z :n-trusted (count trust-g) :test test-g])
            ]
        (if (every? true?
                    (for [x [false true]
                          y [false true]
                          r [false true]
                          :let [wi (build-xy x y r i)
                                wi (loop [wi wi
                                          gs (concat trust-g test-g)]
                                     (if (empty? gs) wi
                                       (let [[[w1 w2 op wo :as g] & gs] gs
                                             v1 (wi w1)
                                             v2 (wi w2)]
                                         (if (or (nil? v1) (nil? v2))
                                           (recur wi (concat gs [g]))
                                           (recur (assoc wi wo (OP op v1 v2)) gs)))))
                                ]]
                      (= (get wi z) (first (sum (if (= 45 i) false x) (if (= 45 i) false y) (if (zero? i) false r))))))
          (recur (concat trust-g test-g) (apply disj un-g test-g) (inc i))
          (prn [:test-g test-g])
          )
          ))
)

    ))

(defn -main
  [& args]
  (println "day24")
  (println sample1)
  (newline)

  (comment
  (println "part1")
  (prn (d24p1 sample1))
  (prn sol1)
  (prn (d24p1 sample2))
  (prn sol2)
  (prn (d24p1 (slurp "input/day24.txt")))
)
  (newline)
  (println "part2")
  ;;  (prn (d24p2 sample))
  (prn (d24p2 (slurp "input/day24-p2.txt")))
  )


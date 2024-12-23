(ns t.d22
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"1
10
100
2024")
(def sol 37327623)

(defn parse-input
  [input]
  (->> input
  (cljstr/split-lines)
  (map parse-long)
  ))


(def mix bit-xor)
(comment
  (mix 42 15)
  ;; 37
  )

(defn prune
  [v]
  (mod v 16777216))
(comment
  (prune 100000000)
  ;; 16113920
  )

(defn op1
  [s]
  (-> s
      (* 64)
      (mix s)
      prune))
(comment
  (op1 123)
  ;; nope : 15887950
  ;; 7867
  )

(defn op2
  [s]
  (-> s
      (quot 32)
      (mix s)
      prune))

(defn op3
  [s]
  (-> s
      (* 2048)
      (mix s)
      prune))

(defn next-sn
  [s]
  (-> s
      op1
      op2
      op3))
(comment
  (next-sn (next-sn 123))
  (prune 123)
  )


(comment
  (def insn (iterate next-sn 1))
  (nth insn 2000)

  )

(defn d22p1
  [input]
  (let [x (parse-input input)
        ]
    (apply +
           (for [sn x]
             (nth (iterate next-sn sn) 2000))
           )))


(def sample2
"1
2
3
2024")
(def sol2 23)

(defn price
  [v]
  (parse-long (str (last (str v)))))
(comment (price 2376))

(defn all-combo
  [sn]
  (let [all-prices (take 2000 (map price (iterate next-sn sn)))
        all-diff (map - (rest all-prices) all-prices )
        ]
    (loop [i 4
           saved {}
           ]
      (if (= i 2000) saved
        (let [combo (take 4 (drop (- i 4) all-diff))
              price (nth all-prices i)]
          (if (contains? saved combo)
            (recur (inc i) saved)
            (recur (inc i) (assoc saved combo price)))
          )))))
(comment
(all-combo 123)
  )



(defn d22p2
  [input]
  (let [x (parse-input input)
        ]
    (loop [sn-s x
           combo-values {}]
      (if (empty? sn-s) (apply max (vals combo-values))
        (let [[sn & sn-s] sn-s
              combos (all-combo sn)]
          (recur sn-s (merge-with + combo-values combos))))

      )
    ))

(defn -main
  [& args]
  (println "day22")
  (println sample)
  (newline)

  (comment
  (println "part1")
  (prn (d22p1 sample))
  (prn sol)
  (prn (d22p1 (slurp "input/day22.txt")))
)
  (newline)
  (println "part2")
  (prn (d22p2 sample2))
  (prn sol2)
  (prn (d22p2 (slurp "input/day22.txt")))
  )


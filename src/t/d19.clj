(ns t.d19
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
")
(def sol 6)

(defn parse-input
  [input]
  (let [[towels patterns] (cljstr/split input #"\n\n")
        towels (map vec (cljstr/split towels #", "))
        patterns (map vec (cljstr/split-lines patterns)) ]
    [towels patterns]))


(defn d19p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d19p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day19")
  (println sample)
  (newline)

  (println "part1")
  (prn (d19p1 sample))
  (prn sol)
  ;;  (prn (d19p1 (slurp "input/day19.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d19p2 sample))
  ;;  (prn (d19p2 (slurp "input/day19.txt")))
  )


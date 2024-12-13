(ns t.d13
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d13p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d13p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day13")
  (println sample)
  (newline)

  (println "part1")
  (prn (d13p1 sample))
  ;;  (prn (d13p1 (slurp "input/day13.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d13p2 sample))
  ;;  (prn (d13p2 (slurp "input/day13.txt")))
  )


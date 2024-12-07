(ns t.d7
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d7p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d7p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day7")
  (println sample)
  (newline)

  (println "part1")
  (prn (d7p1 sample))
  ;;  (prn (d7p1 (slurp "input/day7.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d7p2 sample))
  ;;  (prn (d7p2 (slurp "input/day7.txt")))
  )


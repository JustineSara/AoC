(ns t.d5
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d5p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d5p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day5")
  (println sample)
  (newline)

  (println "part1")
  (prn (d5p1 sample))
  ;;  (prn (d5p1 (slurp "input/day5.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d5p2 sample))
  ;;  (prn (d5p2 (slurp "input/day5.txt")))
  )


(ns t.d6
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

  (def sample "")

  (defn parse-input
  [input]
  (cljstr/split-lines input)
  )


  (defn d6p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d6p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day6")
  (println sample)
  (newline)

  (println "part1")
  (prn (d6p1 sample))
  ;;  (prn (d6p1 (slurp "input/day6.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d6p2 sample))
  ;;  (prn (d6p2 (slurp "input/day6.txt")))
  )
  
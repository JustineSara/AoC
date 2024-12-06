(ns t.d4
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

  (def sample "")

  (defn parse-input
  [input]
  (cljstr/split-lines input)
  )


  (defn d4p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d4p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day4")
  (println sample)
  (newline)

  (println "part1")
  (prn (d4p1 sample))
  ;;  (prn (d4p1 (slurp "input/day4.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d4p2 sample))
  ;;  (prn (d4p2 (slurp "input/day4.txt")))
  )
  
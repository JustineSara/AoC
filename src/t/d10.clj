(ns t.d10
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

  (def sample "")

  (defn parse-input
  [input]
  (cljstr/split-lines input)
  )


  (defn d10p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d10p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day10")
  (println sample)
  (newline)

  (println "part1")
  (prn (d10p1 sample))
  ;;  (prn (d10p1 (slurp "input/day10.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d10p2 sample))
  ;;  (prn (d10p2 (slurp "input/day10.txt")))
  )
  
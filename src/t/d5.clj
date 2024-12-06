(ns t.d5
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

  (def sample "")

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
  
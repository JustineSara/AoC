(ns t.d8
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

  (def sample "")

  (defn parse-input
  [input]
  (cljstr/split-lines input)
  )


  (defn d8p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d8p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day8")
  (println sample)
  (newline)

  (println "part1")
  (prn (d8p1 sample))
  ;;  (prn (d8p1 (slurp "input/day8.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d8p2 sample))
  ;;  (prn (d8p2 (slurp "input/day8.txt")))
  )
  
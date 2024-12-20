(ns t.d20
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


  (defn d20p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d20p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day20")
  (println sample)
  (newline)

  (println "part1")
  (prn (d20p1 sample))
  ;;  (prn (d20p1 (slurp "input/day20.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d20p2 sample))
  ;;  (prn (d20p2 (slurp "input/day20.txt")))
  )
  
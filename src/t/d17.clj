(ns t.d17
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


  (defn d17p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d17p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day17")
  (println sample)
  (newline)

  (println "part1")
  (prn (d17p1 sample))
  ;;  (prn (d17p1 (slurp "input/day17.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d17p2 sample))
  ;;  (prn (d17p2 (slurp "input/day17.txt")))
  )
  
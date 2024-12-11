(ns t.d12
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


  (defn d12p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d12p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day12")
  (println sample)
  (newline)

  (println "part1")
  (prn (d12p1 sample))
  ;;  (prn (d12p1 (slurp "input/day12.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d12p2 sample))
  ;;  (prn (d12p2 (slurp "input/day12.txt")))
  )
  
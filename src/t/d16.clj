(ns t.d16
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


  (defn d16p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d16p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day16")
  (println sample)
  (newline)

  (println "part1")
  (prn (d16p1 sample))
  ;;  (prn (d16p1 (slurp "input/day16.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d16p2 sample))
  ;;  (prn (d16p2 (slurp "input/day16.txt")))
  )
  
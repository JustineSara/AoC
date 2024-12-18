(ns t.d18
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


  (defn d18p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d18p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day18")
  (println sample)
  (newline)

  (println "part1")
  (prn (d18p1 sample))
  ;;  (prn (d18p1 (slurp "input/day18.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d18p2 sample))
  ;;  (prn (d18p2 (slurp "input/day18.txt")))
  )
  
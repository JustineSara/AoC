(ns t.d13
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


  (defn d13p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d13p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day13")
  (println sample)
  (newline)

  (println "part1")
  (prn (d13p1 sample))
  ;;  (prn (d13p1 (slurp "input/day13.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d13p2 sample))
  ;;  (prn (d13p2 (slurp "input/day13.txt")))
  )
  
(ns t.d14
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


  (defn d14p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d14p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day14")
  (println sample)
  (newline)

  (println "part1")
  (prn (d14p1 sample))
  ;;  (prn (d14p1 (slurp "input/day14.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d14p2 sample))
  ;;  (prn (d14p2 (slurp "input/day14.txt")))
  )
  
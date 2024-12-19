(ns t.d19
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


  (defn d19p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d19p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day19")
  (println sample)
  (newline)

  (println "part1")
  (prn (d19p1 sample))
  ;;  (prn (d19p1 (slurp "input/day19.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d19p2 sample))
  ;;  (prn (d19p2 (slurp "input/day19.txt")))
  )
  
(ns t.d7
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

  (def sample "")

  (defn parse-input
  [input]
  (cljstr/split-lines input)
  )


  (defn d7p1
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn d7p2
  [input]
  (let [x (parse-input input)
  ]
  x
  ))

  (defn -main
  [& args]
  (println "day7")
  (println sample)
  (newline)

  (println "part1")
  (prn (d7p1 sample))
  ;;  (prn (d7p1 (slurp "input/day7.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d7p2 sample))
  ;;  (prn (d7p2 (slurp "input/day7.txt")))
  )
  
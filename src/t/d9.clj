(ns t.d9
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"2333133121414131402")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d9p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d9p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day9")
  (println sample)
  (newline)

  (println "part1")
  (prn (d9p1 sample))
  ;;  (prn (d9p1 (slurp "input/day9.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d9p2 sample))
  ;;  (prn (d9p2 (slurp "input/day9.txt")))
  )


(ns t.d10
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn parse-input
  [input]
  (->> (cljstr/split-lines input)
       (map vec)
       (map-indexed
         (fn [y l]
           (->> l
           (map-indexed
             (fn [x c] {c [[x y]]})
             )
           (apply merge-with into))))
       (apply merge-with into)
  ))

(def dirs
  [[0 1] [0 -1] [1 0] [-1 0]])



(defn d10p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d10p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day10")
  (println sample)
  (newline)

  (println "part1")
  (prn (d10p1 sample))
  ;;  (prn (d10p1 (slurp "input/day10.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d10p2 sample))
  ;;  (prn (d10p2 (slurp "input/day10.txt")))
  )


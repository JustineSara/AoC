(ns t.d18
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")
(def sol 22)

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d18p1
  [input smax]
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
  (prn (d18p1 sample 6 12))
  (prn sol)
  ;;  (prn (d18p1 (slurp "input/day18.txt") 70 1024))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d18p2 sample))
  ;;  (prn (d18p2 (slurp "input/day18.txt")))
  )


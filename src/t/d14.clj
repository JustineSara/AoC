(ns t.d14
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(defn parse-input
  [input]
  (->> input
       (re-seq #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
       (map (fn [[_ & r]] (map parse-long r) ))))


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


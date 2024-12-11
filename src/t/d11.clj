(ns t.d11
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"125 17")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )


(defn d11p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d11p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day11")
  (println sample)
  (newline)

  (println "part1")
  (prn (d11p1 sample))
  ;;  (prn (d11p1 (slurp "input/day11.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d11p2 sample))
  ;;  (prn (d11p2 (slurp "input/day11.txt")))
  )


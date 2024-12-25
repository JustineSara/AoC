(ns t.d25
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")
(def sol 3)

(defn parse-input
  [input]
  (->> (cljstr/split input #"\n\n")
       (map cljstr/split-lines)
       (map (fn [ls] {(if (= (ffirst ls) \#) "hole" "key")
                      [(apply map +
                              (map (fn [l] (map (fn [c] (if (= c \#) 1 0)) l) ) ls))]
                      }))
       (apply merge-with concat)
  ))


(defn d25p1
  [input]
  (let [x (parse-input input)
        ]
    (apply +
           (for [h (x "hole")
                 k (x "key")]
             (if (every? #(<= % 7) (map + h k)) 1 0)
             )
           )))

(defn d25p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day25")
  (println sample)
  (newline)

  (println "part1")
  (prn (d25p1 sample))
  (prn sol)
  (prn (d25p1 (slurp "input/day25.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d25p2 sample))
  ;;  (prn (d25p2 (slurp "input/day25.txt")))
  )


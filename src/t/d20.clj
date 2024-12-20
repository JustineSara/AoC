(ns t.d20
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)
        ymax (count lines)
        xmax (count (first lines))
        all-map (->> lines
                     (map-indexed
                       (fn [y l]
                         (apply merge-with into
                                (map-indexed
                                  (fn [x c]
                                    (case c
                                      \S {:start [[x y]]}
                                      \# {:walls [[x y]]}
                                      \E {:end   [[x y]]}
                                      {}))
                                  l))))
                     (apply merge-with into)) ]
    {:walls (set (:walls all-map))
     :S (first (:start all-map))
     :E (first (:end all-map))
     :xmax xmax
     :ymax ymax}
  ))


(defn d20p1
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn d20p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day20")
  (println sample)
  (newline)

  (println "part1")
  (prn (d20p1 sample))
  ;;  (prn (d20p1 (slurp "input/day20.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d20p2 sample))
  ;;  (prn (d20p2 (slurp "input/day20.txt")))
  )


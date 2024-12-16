(ns t.d16
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample1
"###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
")
(def sol1 7036)

(def sample2
"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
")
(def sol2 11048)


(defn parse-input
  [input]
  (->> input
       cljstr/split-lines
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
        (apply merge-with into)
         ))


(defn def-one-step
  [walls move-score turn-score]
  (fn [pos score d]
    (let [n-pos (map + pos d)
          n-dir (if (zero? (first d)) [[1 0] [-1 0]] [[0 1] [0 -1]])
          res (map (fn [nd] [pos (+ score turn-score) nd]) n-dir)]
      (if (contains? walls n-pos) res
        (conj res [n-pos (+ score move-score) d])
      )
    )))



(defn d16p1
  [input]
  (let [{:keys [walls start end]} (parse-input input)
        walls (set walls)
        start (first start)
        end (first end)
        move-score 1
        turn-score 1000
        one-step (def-one-step walls move-score turn-score)
        ]

    #_(let [r [start 0 [1 0]]
          r1 (apply one-step r)
          r1 (sort-by second r1)
          exp #{[start [1 0]]}
          r2 (apply one-step (second r1))
          ]
      (prn r)
      (prn r1)
      (prn (filter (fn [[p _ d]] (not (contains? exp [p d]))) r1))
      (prn r2)
      (prn (filter (fn [[p _ d]] (not (contains? exp [p d]))) r2))
      )

    (loop [runners [[start 0 [1 0]]]
           explored #{[start [1 0]]}]
      (let [[r & runners] runners]
        (if (= (first r) end) (second r)
          (let [n-r (apply one-step r)
                n-r (filter (fn [[p _ d]] (not (contains? explored [p d]))) n-r)
                n-expl (cljset/union explored (set (map (fn [[p _ d]] [p d]) n-r)))]
            (recur (sort-by second (concat runners n-r)) n-expl))
       )
      )
    )))

(defn d16p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day16")
  (println sample1)
  (newline)

  (println "part1")
  (prn (d16p1 sample1))
  (prn [:sol1 sol1])
  (prn (d16p1 sample2))
  (prn [:sol2 sol2])
  (prn (d16p1 (slurp "input/day16.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d16p2 sample))
  ;;  (prn (d16p2 (slurp "input/day16.txt")))
  )


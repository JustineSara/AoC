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
             (fn [x c] {(parse-long (str c)) #{[x y]}})
             )
           (apply merge-with into))))
       (apply merge-with into)
  ))

(def dirs
  [[0 1] [0 -1] [1 0] [-1 0]])

(defn find-next-steps
  [pos next-height-list]
  (for [dir dirs
        :let [new-pos (map + dir pos)]
        :when (contains? next-height-list new-pos)]
    new-pos))

(defn d10p1
  [input]
  (let [x (parse-input input)]
    (loop [paths #{}
           to-explore (map (fn [pos] [0 pos pos]) (get x 0))]
      (if (empty? to-explore)
        (count paths)
        (let [[height pos start-pos] (first to-explore)
              next-h (inc height)
              next-pos (find-next-steps pos (get x next-h))]
          (if (= next-h 9)
            (recur (cljset/union paths (set (map (fn [end-pos] [start-pos end-pos]) next-pos))) (rest to-explore))
            (recur paths (concat (rest to-explore) (map (fn [pos] [next-h pos start-pos]) next-pos)))))))))

(defn d10p2
  [input]
  (let [x (parse-input input)]
    (loop [count-paths 0
           to-explore (map (fn [pos] [0 pos]) (get x 0))]
      (if (empty? to-explore)
        count-paths
        (let [[height pos] (first to-explore)
              next-h (inc height)
              next-pos (find-next-steps pos (get x next-h))]
          (if (= next-h 9)
            (recur (+ count-paths (count next-pos)) (rest to-explore))
            (recur count-paths (concat (rest to-explore) (map (fn [pos] [next-h pos]) next-pos)))))))))

(defn -main
  [& args]
  (println "day10")
  (println sample)
  (newline)

  (println "part1")
  (prn (d10p1 sample))
;;  (prn (d10p1 (slurp "input/day10.txt")))

  (newline)
  (println "part2")
  (prn (d10p2 sample))
  (prn (d10p2 (slurp "input/day10.txt")))
  )


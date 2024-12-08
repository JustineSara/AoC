(ns t.d8
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)
        ymax (dec (count lines))
        xmax (dec (count (first lines)))]
  {:xmax xmax
   :ymax ymax
   :antennas
   (->> lines
        (map-indexed
          (fn [y l] (map-indexed
                      (fn [x c] (when (not= c \.) {c [[x y]]}))
                      l)))
        (reduce concat)
        (filter identity)
        (apply merge-with into)
        )}))


(defn inside?
  [[x y] xmax ymax]
  (and (<= 0 x xmax)
       (<= 0 y ymax)))

(defn antinode
  [[x1 y1] [x2 y2]]
  [[(- (* x1 2) x2) (- (* y1 2) y2)]
   [(- (* x2 2) x1) (- (* y2 2) y1)]])

(defn one-type-antinodes
  [antennas]
  (for [a1 antennas
        a2 antennas
        :when (not= a1 a2)]
    (antinode a1 a2)))

(defn d8p1
  [input]
  (let [{:keys [xmax ymax antennas]} (parse-input input)]
    (->>
      (for [ants (vals antennas)]
        (->>
          (one-type-antinodes ants)
          (reduce concat [])
          set
          (filter (fn[pos] (inside? pos xmax ymax)))))
      (reduce concat [])
      set
      count)))


(defn one-type-antinodes-p2
  [xmax ymax antennas]
  (loop [f-a (first antennas)
         r-a (rest antennas)
         antinodes #{}]
    (if (nil? f-a)
      antinodes
      (let [[ax1 ay1] f-a
            n-antinodes (apply concat
                               (for [[ax2 ay2] r-a]
                                 (concat
                                   (take-while
                                     (fn [pos] (inside? pos xmax ymax))
                                     (map (fn[lambda] [(+ ax1 (* lambda (- ax1 ax2))) (+ ay1 (* lambda (- ay1 ay2)))]) (range)))
                                   (take-while
                                     (fn [pos] (inside? pos xmax ymax))
                                     (map (fn[lambda] [(+ ax2 (* lambda (- ax2 ax1))) (+ ay2 (* lambda (- ay2 ay1)))]) (range))))))]
        (recur (first r-a) (rest r-a) (apply conj antinodes n-antinodes))
        ))))

(defn d8p2
  [input]
  (let [{:keys [xmax ymax antennas]} (parse-input input)]
    (->>
      (for [ants (vals antennas)
            :when (> (count ants) 1)]
        (one-type-antinodes-p2 xmax ymax ants))
      (reduce cljset/union)
      count
    )))

(defn -main
  [& args]
  (println "day8")
  (println sample)
  (newline)

  (println "part1")
  (prn (d8p1 sample))
;;  (prn (d8p1 (slurp "input/day8.txt")))

  (newline)
  (println "part2")
  (prn (d8p2 sample))
  (prn (d8p2 (slurp "input/day8.txt")))
  )


(ns t.d6
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)
        y-max (dec (count lines))
        x-max (dec (count (first lines)))]
    {:y-max y-max
     :x-max x-max
     :map
     (->> lines
          (map-indexed
            (fn [y l] (->>
                        l
                        (map-indexed
                          (fn [x c]
                            (cond
                              (= c \#) {:O (set [[x y]])}
                              (= c \^) {:G [[x y] :up]})))
                        (apply merge-with into)
                        )))
          (apply merge-with into))}))

(defn one-step-move
  [info]
  (let [obstacles (get-in info [:map :O])
        [[gx gy] g-dir] (get-in info [:map :G])]
    (cond
      (= g-dir :up)
      (let [up-pos [gx (dec gy)]
            occupied? (contains? obstacles up-pos)]
        (if occupied?
          (recur (assoc-in info [:map :G 1] :right))
          [(assoc-in info [:map :G 0] up-pos) up-pos]))
      :else
      [-1 -1]
      )))


(defn d6p1
  [input]
  (let [info-start (parse-input input)
        ]
    (loop [info info-start
           Gpos (set [(get-in info-start [:map :G 0])])]
      (let [[n-info n-pos] (one-step-move info)]
        (prn n-pos)
        (if (= n-info -1) nil
          (recur n-info (conj Gpos n-pos)))))
    ))

(defn d6p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day6")
  (println sample)
  (newline)

  (println "part1")
  (prn (d6p1 sample))
  ;;  (prn (d6p1 (slurp "input/day6.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d6p2 sample))
  ;;  (prn (d6p2 (slurp "input/day6.txt")))
  )


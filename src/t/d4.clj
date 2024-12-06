(ns t.d4
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn parse-input
  [input]
  (->> input
       cljstr/split-lines
       (map-indexed (fn [i l]
                      (->> l
                      (map-indexed (fn [ii letter] {letter [[i ii]]}))
                      (apply merge-with into))))
       (apply merge-with into)))


(comment
  all this is useless because I misunderstood the thing that explains the stuff
(defn search-letter-around
  [pos letter-pos]
  (let [x (first pos)
        y (second pos)
        around (for [i [-1 0 +1]
                     ii [-1 0 +1]]
                 [(+ x i) (+ y ii)])]
    (cljset/intersection letter-pos (set around))))

(defn loop-on-S
  [Apos Ss]
  (count (search-letter-around Apos Ss)))

(defn loop-on-A
  [Mpos As Ss]
  (let [as (search-letter-around Mpos As)]
   (loop [a (first as)
          otherAs (rest as)
          tot 0]
     (if (nil? a)
       tot
       (recur (first otherAs) (rest otherAs) (+ tot
                                                (loop-on-S a Ss)))))))

(defn loop-on-M
  [Xpos Ms As Ss]
  (let [ms (search-letter-around Xpos Ms)]
   (loop [m (first ms)
          otherMs (rest ms)
          tot 0]
     (if (nil? m)
       tot
       (recur (first otherMs) (rest otherMs) (+ tot
                                                (loop-on-A m As Ss)))))))
(defn d4p1
  [input]
  (let [p-input (parse-input input)
        Xs (set (get p-input \X))
        Ms (set (get p-input \M))
        As (set (get p-input \A))
        Ss (set (get p-input \S))
        ]
    (loop [x (first Xs)
           otherXs (rest Xs)
           tot 0]
      (if (nil? x)
        tot
      (recur (first otherXs) (rest otherXs) (+ tot
                                                (loop-on-M x Ms As Ss)))))))
)

(defn xmas?
  [[x y] [dx dy] Ms As Ss]
    (and
      (contains? Ms [(+ x dx)       (+ y dy)])
      (contains? As [(+ x dx dx)    (+ y dy dy)])
      (contains? Ss [(+ x dx dx dx) (+ y dy dy dy)])
      ))

(defn d4p1
  [input]
  (let [p-input (parse-input input)
        Xs (set (get p-input \X))
        Ms (set (get p-input \M))
        As (set (get p-input \A))
        Ss (set (get p-input \S))
        dirs (for [dx [+1 0 -1] dy [+1 0 -1]] [dx dy])
        ]
    (loop [x (first Xs)
           otherXs (rest Xs)
           tot 0]
      (if (nil? x)
        tot
        (let [tots (map (fn [dir] (if (xmas? x dir Ms As Ss) 1 0)) dirs)]
          (recur (first otherXs) (rest otherXs) (apply + tot tots)))
      ))))

(defn mas-in-forw
  [Apos Ms Ss]
  (let [x (first Apos)
        y (second Apos)]
    (or
      (and (contains? Ms [(+ x 1) (+ y 1)])
           (contains? Ss [(- x 1) (- y 1)]))
      (and (contains? Ss [(+ x 1) (+ y 1)])
           (contains? Ms [(- x 1) (- y 1)])))))

(defn mas-in-backw
  [Apos Ms Ss]
  (let [x (first Apos)
        y (second Apos)]
    (or
      (and (contains? Ms [(- x 1) (+ y 1)])
           (contains? Ss [(+ x 1) (- y 1)]))
      (and (contains? Ss [(- x 1) (+ y 1)])
           (contains? Ms [(+ x 1) (- y 1)])))))

(defn d4p2
  [input]
  (let [p-input (parse-input input)
        Ms (set (get p-input \M))
        As (set (get p-input \A))
        Ss (set (get p-input \S))
        ]
    (loop [a (first As)
           otherAs (rest As)
           tot 0]
      (if (nil? a)
        tot
        (recur (first otherAs)
               (rest otherAs)
               (if (and (mas-in-forw a Ms Ss) (mas-in-backw a Ms Ss)) (inc tot) tot))))))

(defn -main
  [& args]
  (println "day4")
  (println sample)
  (newline)

  (println "part1")
  (prn (d4p1 sample))
  (prn (d4p1 (slurp "input/day4.txt")))

  (newline)
  (println "part2")
  (prn (d4p2 sample))
  (prn (d4p2 (slurp "input/day4.txt")))
  )


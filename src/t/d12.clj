(ns t.d12
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample1
"AAAA
BBCD
BBCC
EEEC
")
(def sol1 140)
(def sol1p2 80)

(def sample2
"OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")
(def sol2 772)
(def sol2p2 436)

(def sample3
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")
(def sol3 1930)
(def sol3p2 1206)

(def sample4
"EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")
(def sol4p2 236)

(def sample5
"AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")
(def sol5p2 368)


(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)
        xmax (count (first lines))
        ymax (count lines)]
    {:full lines
     :xmax xmax
     :ymax ymax
     :organised
     (->> lines
         (map-indexed
           (fn [y l] (->> l
                          (map-indexed
                            (fn [x c] {c #{[x y]}}))
                          (apply merge-with into))))
         (apply merge-with into))}))

(def dir [[0 1][0 -1][1 0][-1 0]])

(defn find-one
  [s-pos set-of-cs]
  (loop [pos #{s-pos}
         surface 0
         border 0
         inside #{}
         ]
;;    (newline)
;;    (prn [:pos pos :surface surface :border border :inside inside])
    (if (empty? pos)
      [(* surface border) (cljset/difference set-of-cs inside)]
      (let [[x y] (first pos)
            pos (disj pos [x y])
            next-pos (for [[dx dy] dir
                          :let [new-pos [(+ x dx) (+ y dy)]]
                          :when (contains? set-of-cs new-pos)]
                      new-pos)
            n-border (- (count dir) (count next-pos))
            next-pos (filter (fn [p] (not (contains? inside p))) next-pos)]
;;        (prn [:xy [x y] :pos pos :new-pos next-pos :S (inc surface) :n-B n-border])
        (recur
          (apply conj pos next-pos)
          (inc surface)
          (+ border n-border)
          (conj inside [x y])
               )))))

(defn d12p1
  [input]
  (let [{:keys [organised]} (parse-input input) ]
    #_(find-one [0 1] (get organised \B))
    (apply +
           (for [c (keys organised)
                 :let [set-of-cs (get organised c)]]
             (loop [c-s set-of-cs
                    sol 0]
               (if (empty? c-s)
                 sol
                 (let [s-pos (first c-s)
                       [n-sol n-c-s] (find-one s-pos c-s)]
                   (recur n-c-s (+ sol n-sol)))))))))

(defn find-borders
  [[x y] c-s]
  (let [ borders (filter (fn [[dx dy]] (not (contains? c-s [(+ x dx) (+ y dy)]))) dir)]
    (->> borders
         (map
           (fn [[dx dy]] (cond
                           (= 1 dx) {:vp [[(inc x) y]]}
                           (= -1 dx) {:vm [[x y]]}
                           (= 1 dy) {:hp [[x (inc y)]]}
                           (= -1 dy) {:hm [[x y]]})))
         (apply merge-with into))))

(defn count-borders
  [borders fn1 fn2]
  (reduce +
  (for [x (set (map fn1 borders))
        :let [ys (->> borders
                      (filter (fn [p] (= (fn1 p) x)))
                      (map fn2)
                      sort)
              diffs (map - (rest ys) ys)]]
   ;; (do
     ;; (prn [:x x :ys ys])
  ;;    (prn [:diffs diffs])
      (inc (count (filter (fn[d] (< 1 d)) diffs)))
 ;;     )
    )
  ))

(defn find-one-p2
  [s-pos set-of-cs]
  (loop [pos #{s-pos}
         surface 0
         borders {}
         inside #{}
         ]
;;    (newline)
;;    (prn [:pos pos :surface surface :borders borders :inside inside])
    (if (empty? pos)
;;      (do
;;        (prn [:S surface])
;;       (prn [:B [:h (count-borders (:h borders) second first)]])
;;       (prn [:B [:v (count-borders (:v borders) first second)]])
        [(* surface
            (+ (count-borders (:hm borders) second first)
               (count-borders (:hp borders) second first)
               (count-borders (:vp borders) first second)
               (count-borders (:vm borders) first second)))
         (cljset/difference set-of-cs inside)]
;;        )
      (let [[x y] (first pos)
            pos (disj pos [x y])
            next-pos (for [[dx dy] dir
                          :let [new-pos [(+ x dx) (+ y dy)]]
                          :when (contains? set-of-cs new-pos)
                          :when (not (contains? inside new-pos))]
                      new-pos)
            n-borders (find-borders [x y] set-of-cs)]
;;        (prn [:xy [x y] :pos pos :new-pos next-pos :S (inc surface) :n-B n-borders])
        (recur
          (apply conj pos next-pos)
          (inc surface)
          (merge-with into borders n-borders)
          (conj inside [x y])
               )))))
(defn d12p2
  [input]
  (let [{:keys [organised]} (parse-input input)
        ]
;;    (prn [:h [[3 2] [3 1]] :second :first])
;;    (count-borders [[3 2] [3 1]] second first)
    #_(find-one-p2 [3 1] (get organised \D))
    (apply +
           (for [c (keys organised)
                 :let [set-of-cs (get organised c)]]
             (loop [c-s set-of-cs
                    sol 0]
               (if (empty? c-s)
      ;;           (do (prn [c sol])
                     sol
      ;;               )
                 (let [s-pos (first c-s)
                       [n-sol n-c-s] (find-one-p2 s-pos c-s)]
                   (recur n-c-s (+ sol n-sol)))))))
    ))

(defn -main
  [& args]
  (println "day12")
  (println sample1)
  (newline)

;;  (println "part1")
;;  (prn (d12p1 sample1))
;;  (prn sol1)
;;  (prn (d12p1 sample2))
;;  (prn sol2)
;;  (prn (d12p1 sample3))
;;  (prn sol3)
;;  (prn (d12p1 (slurp "input/day12.txt")))

  (newline)
  (println "part2")
  (prn (d12p2 sample1))
  (prn sol1p2)
  (prn (d12p2 sample2))
  (prn sol2p2)
  (prn (d12p2 sample3))
  (prn sol3p2)
  (prn (d12p2 sample4))
  (prn sol4p2)
  (prn (d12p2 sample5))
  (prn sol5p2)
  (prn (d12p2 (slurp "input/day12.txt")))
  ;; 887536 is too low
  )


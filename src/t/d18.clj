(ns t.d18
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")
(def sol 22)

(defn parse-input
  [input]
  (->> input
       (re-seq #"(\d+),(\d+)")
       (map (fn [[_ x y]] [(parse-long x) (parse-long y)]))
       ))

(defn corrupted-mem
  [fall nfall]
  (set (take nfall (cycle fall))))

(def dir [[0 1] [0 -1] [1 0] [-1 0]])
(defn find-way
  [start end obst smax]
  (loop [pos [start]
         alreadythere #{start}
         steps 0]
    (if (empty? pos) -1
;;    (prn [:steps steps :nexpl (count alreadythere) :npos (count pos)])
    (let [npos (->> pos
                    (mapcat (fn [p] (map (fn [d] (map + p d)) dir) ))
                    (distinct)
                    (filter (fn [p] (every? #(<= 0 % smax) p)))
                    (filter (fn [p] (not (contains? obst p))))
                    (filter (fn [p] (not (contains? alreadythere p))))
                    )]
      (if (some #(= % end) npos) (inc steps)
        (recur npos (apply conj alreadythere npos) (inc steps)))
      ))))

(defn d18p1
  [input smax nbytes]
  (let [fall (parse-input input)
        c-mem (corrupted-mem fall nbytes)
        ]
    (find-way [0 0] [smax smax] c-mem smax)))



(defn d18p2
  [input smax nbytes]
  (let [fall (parse-input input)
        c-mem (corrupted-mem fall nbytes)
        r-fall (drop nbytes fall)
        ]
    (loop [falling r-fall
           c-mem c-mem]
      (let [[nf & falling] falling
            c-mem (conj c-mem nf)]
        (if (pos? (find-way [0 0] [smax smax] c-mem smax))
          (recur falling c-mem)
          nf)))))

(defn -main
  [& args]
  (println "day18")
  (println sample)
  (newline)

  (comment
  (println "part1")
  (prn (d18p1 sample 6 12))
  (prn sol)
  (prn (d18p1 (slurp "input/day18.txt") 70 1024))
)

  (newline)
  (println "part2")
  (prn (d18p2 sample 6 12))
  (prn "6,1")
  (prn (d18p2 (slurp "input/day18.txt") 70 1024))
  )


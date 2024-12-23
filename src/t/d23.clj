(ns t.d23
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defn parse-input
  [input]
  ( update-vals
    (->>
      (cljstr/split-lines input)
      (map (fn [s] (let [[c1 c2] (cljstr/split s #"-")]
                     {c1 [c2] c2 [c1]})))
      (apply merge-with concat))
    set))


(defn d23p1
  [input]
  (let [x (parse-input input)
        cs (keys x)
        ts (->> cs
                (filter (fn [s] (= (first s) \t))))
        ]
    (->>
      (for [t ts
            c1 (x t)
            c2 (x t)
            :when (not= c1 c2)
            :when (contains? (x c1) c2)]
        #{t c1 c2})
      set
      count)
    ))

(defn d23p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day23")
  (println sample)
  (newline)

  (println "part1")
  (prn (d23p1 sample))
  (prn (d23p1 (slurp "input/day23.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d23p2 sample))
  ;;  (prn (d23p2 (slurp "input/day23.txt")))
  )


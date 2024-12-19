(ns t.d19
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
")
(def sol 6)

(defn parse-input
  [input]
  (let [[towels patterns] (cljstr/split input #"\n\n")
        towels (map vec (cljstr/split towels #", "))
        patterns (map vec (cljstr/split-lines patterns)) ]
    [towels patterns]))


(defn p-valid?
  [p T]
  (let [psize (count p)
        t-exact (some (fn [t] (and
                                (= psize (count t))
                                (= t p))) T)
        t-smaller (filter (fn [t] (and
                                    (< (count t) psize)
                                    (every? identity (map = t p))))
                                    T)
        ]
;;    (prn [:p p :finish t-exact :t-smaller t-smaller])
    (or
      t-exact
      (some
        (fn[t] (p-valid? (drop (count t) p) T))
        t-smaller)
    ))
  )

(defn d19p1
  [input]
  (let [[T P] (parse-input input)
        p (first P)
        ]
    (->> P
         (keep (fn [p] (p-valid? p T)))
         count)

    ))


(defn count-arr
  [p T]
  (let [psize (count p)
        t-exact (some (fn [t] (and
                                (= psize (count t))
                                (= t p))) T)
        t-smaller (filter (fn [t] (and
                                    (< (count t) psize)
                                    (every? identity (map = t p))))
                                    T)
        ]
    (+
     (if t-exact 1 0)
     (apply +
            (map
                (fn[t] (count-arr (drop (count t) p) T))
                t-smaller)
              ))
     ))

(defn d19p2
  [input]
  (let [[T P] (parse-input input)
        p (first P)
        ]
    (apply +
    (map (fn [p] (count-arr p T)) P)
    )))

(defn -main
  [& args]
  (println "day19")
  (println sample)
  (newline)

  (comment
  (println "part1")
  (prn (d19p1 sample))
  (prn sol)
  (prn (d19p1 (slurp "input/day19.txt")))
  )

  (newline)
  (println "part2")
  (prn (d19p2 sample))
  (prn 16)
  (prn (d19p2 (slurp "input/day19.txt")))
  )


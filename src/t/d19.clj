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
        towels (cljstr/split towels #", ")
        patterns (cljstr/split-lines patterns)]
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
        T (map vec T)
        P (map vec P)
        p (first P)
        ]
    (->> P
         (keep (fn [p] (p-valid? p T)))
         count)

    ))



(defn count-arr-M
  [T]
  (memoize
    (defn count-arr
      [p]
      (let [psize (count p)
            t-forward (filter (fn [t] (and
                                      (< (count t) psize)
                                      (every? identity (map = t p))))
                            T)
            t-exact (filter (fn[t] (= t p)) T)
            ]
        (+ (count t-exact)
           (apply +
                  (map
                    (fn[t] (count-arr (drop (count t) p)))
                    t-forward)))
        ))))

(defn d19p2-no-patience
  [input]
  (let [[T P] (parse-input input)
        p (first P)
        countArr (count-arr-M T)
        ]
    (loop [Ps [p]
           Narr 0]
      (prn [:countPs (count Ps) :Narr Narr])
      (if (empty? Ps) Narr
        (let [[p & Ps] Ps
              ]
          (recur Ps (+ Narr (countArr p)))
          ))
      )
    ))



(defn count-arr
  [p n T]
  (let [psize (count p)
        t-valid (filter (fn [t] (and
                                  (<= (count t) psize)
                                  (every? identity (map = t p))))
                        T)
        ]
    (apply merge-with +
    (map
      (fn[t] {(drop (count t) p) n})
      t-valid))
    ))

(defn d19p2
    "trying-to-group-unfinished"
  [input]
  (let [[T P] (parse-input input)
        p (first P)
        ]
    (loop [Ps (apply merge-with + (map (fn [p] {p 1}) P))
           Narr 0]
;;      (prn [:Ps Ps :Narr Narr])
      (if (empty? Ps) Narr
        (let [largest-p (first (sort-by count > (keys Ps)))
              p-n (get Ps largest-p)
              np (count-arr largest-p p-n T)
              v (get np [] (get np '() 0))
              np (apply dissoc np [[] '()])
              ]
          (comment
            (prn largest-p p-n)
            (prn np v)
            (read-line)
            )
          (recur (merge-with + (dissoc Ps largest-p) np) (+ Narr v))
          ))
      )
    ))

(defn d19p2-hmph
  [input]
  (let [[T P] (parse-input input)
        p-max-size (apply max (map count P))
        T (->> T
               (map (fn [t] {(count t) {t 1}} ))
               (apply merge-with merge)
               )
        ]
    p-max-size
    T
    ))

(defn all-arr
  [p T]
  (loop [arr-s T
         N 0]
    (if (empty? arr-s) N
      (let [n-arrs (->> arr-s
                        (mapcat
                          (fn [a] (map
                                    (fn [t] (concat a t))
                                    T)))
                        (filter #(every? true? (map = % p))))
            n-arrs-incomplete (filter #(< (count %) (count p)) n-arrs)
            n-arrs-complete (filter #(= (count %) (count p)) n-arrs)
            ]
        (recur n-arrs-incomplete (+ N (count n-arrs-complete)))
        )
     )))

(comment
(def allT (atom))
(defn split-p
  [p]
  (if-let [N (get @allT p)]
    N
    (let [N (apply +
                   (map
                     (fn [i] (+ (split-p (take i p)) (split-p (drop i p))))
                     (range 1 (count p))))]
      (swap! allT ())))
  )

(defn d19p2
  [input]
  (let [[T P] (parse-input input)
        p (first P)
        ]
    (->> P
         (map (fn [p]
    (all-arr p T)))
         (apply +))

    ))
)

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


(ns t.d3
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
  ;;"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  )

(defn parse-input
  [input]
  input
  )


(defn d3p1
  [input]
  (let [muls (re-seq #"mul\(([0-9]{1,4}),([0-9]{1,4})\)" input)
        ]
    (->> muls
    (map (fn [[_ x1 x2]] (* (parse-long x1) (parse-long x2))))
    (apply +))))

(defn d3p2
  [input]
  (let [instructions (re-seq #"mul\(([0-9]{1,4}),([0-9]{1,4})\)|do\(\)|don\'t\(\)" input)
        ]
    (loop [inst (first instructions)
           inst-s (rest instructions)
           do? true
           tot 0]
      (if (nil? inst)
        tot
        (let [inst-t-n (count (first inst))
              ismul? (> inst-t-n 7)
              x1 (second inst)
              x2 (nth inst 2)
              isdo? (= inst-t-n 4)
              isdont? (= inst-t-n 7)]
          (cond
            (and ismul? do?)
            (recur (first inst-s) (rest inst-s) do? (+ tot (* (parse-long x1) (parse-long x2))))
            isdo?
            (recur (first inst-s) (rest inst-s) true tot)
            isdont?
            (recur (first inst-s) (rest inst-s) false tot)
            :else
            (recur (first inst-s) (rest inst-s) do? tot)
    ))))))

(defn -main
  [& args]
  (println "day3")
  (println sample)
  (newline)

  (println "part1")
  (prn (d3p1 sample))
  ;; (prn (d3p1 (slurp "input/day3.txt")))

  (newline)
  (println "part2")
  (prn (d3p2 sample))
    (prn (d3p2 (slurp "input/day3.txt")))
  )


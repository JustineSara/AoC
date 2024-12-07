(ns t.d7
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input
  [input]
  (->> input
       cljstr/split-lines
       (map (fn [line]
              (let [[result list-nums] (cljstr/split line #": ")
                    nums (map parse-long (cljstr/split list-nums #" "))]
                {:result (parse-long result)
                 :nums nums})))))


(defn test-2-nums
  [x1 x2 res]
  (let [mult (* x1 x2)
        addi (+ x1 x2)]
    (cond
      (= mult res) true
      (= addi res) true
      :else false)
    )
  )

(defn d7p1
  [input]
  (let [x (parse-input input)
        xx (first x)]
    (prn xx)
    (test-2-nums (first (:nums xx)) (second (:nums xx)) (:result xx))
    ))

(defn d7p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day7")
  (println sample)
  (newline)

  (println "part1")
  (prn (d7p1 sample))
  ;;  (prn (d7p1 (slurp "input/day7.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d7p2 sample))
  ;;  (prn (d7p2 (slurp "input/day7.txt")))
  )


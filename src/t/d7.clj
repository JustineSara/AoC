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


(defn test-recur
  [x xs res]
  (if (empty? xs)
    (= x res)
    (let [x2 (first xs)
          xsrest (rest xs)
          mult (* x x2)
          addi (+ x x2)]
      (cond-> false
        (<= mult res) (or (test-recur mult xsrest res))
        (<= addi res) (or (test-recur addi xsrest res))
      ))
  ))

(defn d7p1
  [input]
  (let [x (parse-input input)]
    (->> x
         (map (fn [xx]
                [(test-recur (first (:nums xx)) (rest (:nums xx)) (:result xx))
                 (:result xx)]))
         (filter first)
         (map second)
         (apply +)
    )))

(defn test-p2
  [x xs res]
  (if (empty? xs)
    (= x res)
    (let [x2 (first xs)
          xsrest (rest xs)
          mult (* x x2)
          addi (+ x x2)
          conc (parse-long (str x x2))]
      (cond-> false
        (<= mult res) (or (test-p2 mult xsrest res))
        (<= addi res) (or (test-p2 addi xsrest res))
        (<= conc res) (or (test-p2 conc xsrest res))))))

(defn d7p2
  [input]
  (let [x (parse-input input)
        xx (nth x 4)]
    (->> x
         (map (fn [xx]
                [(test-p2 (first (:nums xx)) (rest (:nums xx)) (:result xx))
                 (:result xx)]))
         (filter first)
         (map second)
         (apply +))))

(defn -main
  [& args]
  (println "day7")
  (println sample)
  (newline)

;;  (println "part1")
;;  (prn (d7p1 sample))
  ;;  (prn (d7p1 (slurp "input/day7.txt")))

  (newline)
  (println "part2")
  (prn (d7p2 sample))
  (prn (d7p2 (slurp "input/day7.txt")))
  )


(ns t.d13
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(defn parse-input
  [input]
   (->>
     (re-seq #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)" input)
     (map (fn [m] (rest m)))
     (map (fn [m] (map parse-long m)))
;;     (map (fn [[_ xA yA xB yB X Y]] {:A [xA yA] :B [xB yB] :P [X Y]}))
    ))

(defn win-prize?
  [ia ib [xa ya xb yb X Y]]
  (and
    (= X (+ (* ia xa) (* ib xb)))
    (= Y (+ (* ia ya) (* ib yb)))))

(defn test-one-machine
  [m]
  (let [win-costs
        (for [iA (range 100)
              iB (range 100)
              :when (win-prize? iA iB m) ]
          (+ (* 3 iA) (* 1 iB)))]
    (if (empty? win-costs) 0
        (apply min win-costs))))

(defn d13p1
  [input]
  (let [machines (parse-input input)
        m (first machines)
        ]
    (->> machines
         (map test-one-machine)
         (apply +))))


(defn iA-and-iB
  [[xa ya xb yb X Y :as m]]
  (let [iB (quot (- (* Y  xa) (* ya X ))
                 (- (* yb xa) (* ya xb)))
        iA (quot (- X (* iB xb))
                 xa)]
    [iA iB m]))

(defn cost
  [[iA iB _]]
  (+ (* 3 iA) (* 1 iB)))

(defn d13p2
  [input]
  (let [machines (parse-input input)
        ]
    (->> machines
         (map (fn [[xa ya xb yb X Y]] [xa ya xb yb (+ 10000000000000 X) (+ 10000000000000 Y)]))
         (map iA-and-iB)
         (filter (comp pos? second))
         (filter (comp pos? first))
         (filter (fn [[iA iB m]] (win-prize? iA iB m)))
         (map cost)
         (apply +)
         )
    ))

(defn -main
  [& args]
  (println "day13")
  (println sample)
  (newline)

  (println "part1")
  (prn (d13p1 sample))
;;  (prn (d13p1 (slurp "input/day13.txt")))

  (newline)
  (println "part2")
  (prn (d13p2 sample))
  (prn (d13p2 (slurp "input/day13.txt")))
  )


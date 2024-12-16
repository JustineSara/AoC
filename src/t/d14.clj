(ns t.d14
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(defn parse-input
  [input]
  (->> input
       (re-seq #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
       (map (fn [[_ & r]] (map parse-long r) ))))

(defn teleport-1d
  [outsidex xmax]
  ;; x and y are not necessarily both outside
  ;; can treat directions separatly
  (cond
    (>= outsidex xmax) (- outsidex xmax)
    (< outsidex 0) (+ xmax outsidex)
    :else outsidex))

(defn max-steps-1d
  [x dx xmax]
  (if (pos? dx)
    (quot (- (dec xmax) x) dx)
    (quot x (abs dx))
    )
  )

(defn steps-and-1tp
  [x y dx dy xmax ymax steps]
  (let [max-steps-in (min
                       (max-steps-1d x dx xmax)
                       (max-steps-1d y dy ymax))]
    (if
      (>= max-steps-in steps) [(+ x (* dx steps)) (+ y (* dy steps)) 0]
      (let [x-outside (+ x (* dx (inc max-steps-in)))
            y-outside (+ y (* dy (inc max-steps-in)))]
        [(teleport-1d x-outside xmax) (teleport-1d y-outside ymax) (- steps max-steps-in 1)]))))

(defn all-steps
  [x y dx dy xmax ymax Nsteps]
  (loop [steps Nsteps
         lx x
         ly y]
    (if (zero? steps)
      [lx ly]
      (let [[nx ny nsteps] (steps-and-1tp lx ly dx dy xmax ymax steps)]
;;        (prn [:prev [:pos lx ly :steps steps :dir dx dy]])
;;        (prn [:next [:pos nx ny :steps nsteps :dir dx dy]])
        (recur nsteps nx ny)))))

(defn draw-1robot
  [x y xmax ymax]
  (->>
    (for [Y (range ymax)]
      (->> (range xmax)
      (map (fn [X] (if (and (= X x)
                                  (= Y y)) "R" ".")) )
      (apply str)
            ))
    (interpose "\n")
    (apply str)))

(defn loop-draw
  [x y dx dy xmax ymax Nsteps]
  (loop [steps Nsteps
         xl x
         yl y]
    (if (zero? steps)
      [xl yl]
      (let [[nx ny nsteps] (steps-and-1tp xl yl dx dy xmax ymax steps)]
        (newline)
        (prn [:steps-left nsteps :dx dx :dy dy :x nx :y ny])
        (print (draw-1robot nx ny xmax ymax))
        (recur nsteps nx ny))))
  )

(defn sum-quad
  [midx midy fnx fny points]
  (->> points
       (filter (fn [[x y]] (and (fnx x midx)
                                (fny y midy))))
       count
       ))

(defn d14p1
  [input xmax ymax]
  (let [robots (parse-input input)
        Nsteps 100
        midx (quot (dec xmax) 2)
        midy (quot (dec ymax) 2)
;;        [x y dx dy] [2 4 2 -3]
;;        [x2 y2 s2] (steps-and-1tp x y dx dy xmax ymax Nsteps)
final-robots (map (fn [[x y dx dy]] (all-steps x y dx dy xmax ymax Nsteps)) robots)
        ]
;;    (prn [:robot x y dx dy])
;;    (prn [:steps Nsteps])
;;    (prn [:steps+1TP (steps-and-1tp x y dx dy xmax ymax Nsteps)])
;;    (prn [:steps+1TP (steps-and-1tp x2 y2 dx dy xmax ymax s2)])
;;    (prn [:all-steps (all-steps x y dx dy xmax ymax Nsteps)])
;;    (loop-draw x y dx dy xmax ymax Nsteps)
(* (sum-quad midx midy < < final-robots)
 (sum-quad midx midy > < final-robots)
 (sum-quad midx midy < > final-robots)
 (sum-quad midx midy > > final-robots)
 )
    ))

(defn one-step
  [x y dx dy xmax ymax]
  [(teleport-1d (+ x dx) xmax)
   (teleport-1d (+ y dy) ymax)
   dx
   dy])

(defn draw-robots
  [robots xmax ymax]
  (let [pos (set (map (fn [[x y _ _]] [x y])  robots))]
  (->>
    (for [Y (range ymax)]
      (->> (range xmax)
      (map (fn [X] (if (contains? pos [X Y]) "R" ".")) )
      (apply str)
            ))
    (interpose "\n")
    (apply str))))

(defn loop-draw-p2
  [robots-i xmax ymax low-steps]
  (loop [steps 0
         robots robots-i
         useri ""]
    (if (pos? (count useri))
      [:steps steps]
      (let [new-robots (map (fn [[x y dx dy]] (one-step x y dx dy xmax ymax)) robots)
            n-steps (inc steps)]
        (prn [:steps n-steps])
        (print (draw-robots new-robots xmax ymax))
        (println)
        (println)
        (println)
        (if (> n-steps low-steps)
          (recur n-steps new-robots (read-line))
          (recur n-steps new-robots useri)))))
  )


(defn d14p2
  [input xmax ymax low-steps]
  (let [robots (parse-input input)
        ]
    (loop-draw-p2 robots xmax ymax low-steps)
    ))

(defn -main
  [& args]
  (println "day14")
  (println sample)
  (newline)

  (println "part1")
  (prn (d14p1 sample 11 7))
  (prn (d14p1 (slurp "input/day14.txt") 101 103))

  (newline)
  (println "part2")
;;  (prn (d14p2 sample 11 7))
  (prn (d14p2 (slurp "input/day14.txt") 101 103 5000))
  )


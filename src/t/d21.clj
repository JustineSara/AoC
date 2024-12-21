(ns t.d21
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"029A
980A
179A
456A
379A")

(defn parse-input
  [input]
  (cljstr/split-lines input)
  )




(def cost-arrow
  {[:a :u] 2
   [:a :r] 2
   [:a :d] 3
   [:a :l] 4
   [:a :a] 1
   [:u :a] 2
   [:u :r] 3
   [:u :d] 2
   [:u :l] 3
   [:u :u] 1
   [:r :a] 2
   [:r :d] 2
   [:r :u] 3
   [:r :l] 3
   [:r :r] 1
   [:d :a] 3
   [:d :u] 2
   [:d :r] 2
   [:d :l] 2
   [:d :d] 1
   [:l :a] 4
   [:l :u] 3
   [:l :r] 3
   [:l :d] 2
   [:l :l] 1
   }
  )

(def pos-arrow
  {:a [2 0]
   :u [1 0]
   :l [0 1]
   :d [1 1]
   :r [2 1]
   })

(defn cost-from-s-to-e
  [[sx sy] [ex ey] cost [no-x no-y]]
  (let [
        delta-x (- ex sx)
        left-right (if (pos? delta-x) :r :l)
        delta-y (- ey sy)
        up-down (if (pos? delta-y) :d :u)
        ]
    (prn [:Dx delta-x :Dy delta-y])
    (prn [:lorr left-right :uord up-down])
    (cond
      (= 0 delta-x delta-y) 1
      (zero? delta-x) (+ (cost [:a up-down])
                         (* (dec (abs delta-y)) (cost [up-down up-down]))
                         (cost [up-down :a])
                         )
      (zero? delta-y) (+ (cost [:a left-right] )
                         (* (dec (abs delta-x)) (cost [left-right left-right]))
                         (cost [left-right :a])
                         )

      (and (= sy no-y) (= ex no-x))
      (do (prn "case sx=nox and ey=noy")
          (prn (cost [:a up-down] )
               (* (- (abs delta-y) 1) (cost [up-down up-down]))
               (cost [up-down left-right])
               (* (- (abs delta-x) 1) (cost [left-right left-right]))
               (cost [left-right :a]))
          (+ (cost [:a up-down] )
             (* (- (abs delta-y) 1) (cost [up-down up-down]))
             (cost [up-down left-right])
             (* (- (abs delta-x) 1) (cost [left-right left-right]))
             (cost [left-right :a]))
          )

      (and (= sx no-x) (= ey no-y))
      (do (prn "case sy=noy and ex=nox")
          (prn (cost [:a left-right] )
               (* (- (abs delta-x) 1) (cost [left-right left-right]))
               (cost [left-right up-down])
               (* (- (abs delta-y) 1) (cost [up-down up-down]))
               (cost [up-down :a]))
          (+ (cost [:a left-right] )
             (* (- (abs delta-x) 1) (cost [left-right left-right]))
             (cost [left-right up-down])
             (* (- (abs delta-y) 1) (cost [up-down up-down]))
             (cost [up-down :a]))
          )

      :else
      (min
        (+ (cost [:a left-right] )
           (* (- (abs delta-x) 1) (cost [left-right left-right]))
           (cost [left-right up-down])
           (* (- (abs delta-y) 1) (cost [up-down up-down]))
           (cost [up-down :a])
           )
        (+ (cost [:a up-down] )
           (* (- (abs delta-y) 1) (cost [up-down up-down]))
           (cost [up-down left-right])
           (* (- (abs delta-x) 1) (cost [left-right left-right]))
           (cost [left-right :a])
           ))
      )))

(defn all-dist
  [pos cost [no-x no-y]]
  (into {}
  (for [s (keys pos)
        e (keys pos)
        :let [[sx sy] (pos s)
              [ex ey] (pos e)
              delta-x (- ex sx)
              left-right (if (pos? delta-x) :r :l)
              delta-y (- ey sy)
              up-down (if (pos? delta-y) :d :u)
              ]]
    (cond
      (= 0 delta-x delta-y) {[s e] 1}
      (zero? delta-x) {[s e] (+ (cost [:a up-down])
                                (* (dec (abs delta-y)) (cost [up-down up-down]))
                                (cost [up-down :a])
                                )}
      (zero? delta-y) {[s e] (+ (cost [:a left-right] )
                                (* (dec (abs delta-x)) (cost [left-right left-right]))
                                (cost [left-right :a])
                                )}
      (and (= sy no-y) (= ex no-x))
      {[s e]
       (+ (cost [:a up-down] )
          (* (- (abs delta-y) 1) (cost [up-down up-down]))
          (cost [up-down left-right])
          (* (- (abs delta-x) 1) (cost [left-right left-right]))
          (cost [left-right :a]))}

      (and (= sx no-x) (= ey no-y))
      {[s e]
       (+ (cost [:a left-right] )
          (* (- (abs delta-x) 1) (cost [left-right left-right]))
          (cost [left-right up-down])
          (* (- (abs delta-y) 1) (cost [up-down up-down]))
          (cost [up-down :a]))}

      :else {[s e]
             (min
               (+ (cost [:a left-right] )
                  (* (- (abs delta-x) 1) (cost [left-right left-right]))
                  (cost [left-right up-down])
                  (* (- (abs delta-y) 1) (cost [up-down up-down]))
                  (cost [up-down :a])
                  )
               (+ (cost [:a up-down] )
                  (* (- (abs delta-y) 1) (cost [up-down up-down]))
                  (cost [up-down left-right])
                  (* (- (abs delta-x) 1) (cost [left-right left-right]))
                  (cost [left-right :a])
                  ))
               }
               ))))

(comment
"
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
"        )

(def human-cost
  (into {}
        (for [s [:a :u :d :l :r]
              e [:a :u :d :l :r]]
          {[s e] 1}
          )))
(comment
  (prn (human-cost [:a nil]))
(all-dist pos-arrow cost-arrow)
(all-dist pos-arrow human-cost)
  )

(comment
"
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
")
(def pos-num
  {\A [2 3]
   \1 [0 2]
   \2 [1 2]
   \3 [2 2]
   \4 [0 1]
   \5 [1 1]
   \6 [2 1]
   \7 [0 0]
   \8 [1 0]
   \9 [2 0]
   \0 [1 3]
   })


(def robot1-cost (all-dist pos-arrow human-cost [0 0]))
(for [k (keys cost-arrow)]
  (do
    (prn k)
  (prn [:userdef (cost-arrow k)])
  (prn [:compdef (robot1-cost k)]))
  )
(def  robot2-cost (all-dist pos-arrow robot1-cost [0 0]))
(def  robot3-cost (all-dist pos-num robot2-cost [0 3]))

(comment
  (cost-from-s-to-e [2 0] [1 0] robot1-cost [0 0])
 (robot3-cost [\A \1])
(robot2-cost [:l :a])
  (cost-from-s-to-e [2 3] [0 2] robot2-cost [0 3])
 )

(defn cost-one-code
  [c]
    (->> c
         (str "A")
         (partition 2 1)
         (map robot3-cost)
         (apply +))
  )
(defn value-one-code
  [c]
  (parse-long
       (cljstr/replace c "A" "")))
(comment
(parse-long (cljstr/replace "029A" "A" ""))
(value-one-code "029A")
)

(defn d21p1
  [input]
  (let [codes (parse-input input)
        cost-codes (map cost-one-code codes)
        value-codes (map value-one-code codes)
        ]
    (prn (robot3-cost [\A \1]))
    (prn cost-codes)
    (prn value-codes)
    (reduce + (map * cost-codes value-codes))
    ))

(defn d21p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day21")
  (println sample)
  (newline)

  (println "part1")
  (prn (d21p1 sample))
  (prn (d21p1 (slurp "input/day21.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d21p2 sample))
  ;;  (prn (d21p2 (slurp "input/day21.txt")))
  )


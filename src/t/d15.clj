(ns t.d15
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample1
"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(def sol1 2028)

(def sample2
"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")
(def sol2 10092)
(def sol2p2 9021)

(def sample3
"#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")
(def sol3p2 (+ 105 2 700 3 600))

(defn parse-input
  [input]
  (let [[warehouse moves] (cljstr/split input #"\n\n")
        warehouse (->> warehouse
                       cljstr/split-lines
                       (map-indexed (fn[y l]
                                      (apply merge-with into
                                      (map-indexed
                                        (fn [x c] (case c
                                                    \# {:walls #{[x y]}}
                                                    \O {:box #{[x y]}}
                                                    \@ {:robot #{[x y]}}
                                                    {}))
                                        l))))
                       (apply merge-with into))
        moves (-> moves
                  (cljstr/replace "\n" ""))]
  [warehouse moves]))

(defn symb-to-dir
  [symb]
  (case symb
    \> [+1 0]
    \< [-1 0]
    \v [0 +1]
    \^ [0 -1]))

(defn robot-move-box
  [robot box1 dir box walls]
  (loop [might-move #{}
         try-move box1
         boxes box]
    (let [npos (map + try-move dir)]
      (cond
        (contains? boxes npos)
        (recur (cljset/union might-move #{try-move}) npos (disj boxes npos))
        (contains? walls npos)
        [robot (cljset/union might-move #{try-move} boxes)]
        :else
        [(map + robot dir) (->> try-move
                                (conj might-move)
                                (map (fn [b] (map + b dir)))
                                (set)
                                (cljset/union boxes))]
        ))
    ))


(defn one-step
  [robot symb box walls]
  (let [dir (symb-to-dir symb)
        n-robot (map + robot dir)]
    ;; try to move the robot
    (cond
      (contains? walls n-robot) [robot box]
      (contains? box n-robot) (robot-move-box robot n-robot dir (disj box n-robot) walls)
      :else [n-robot box])))


(defn d15p1
  [input]
  (let [[{:keys [walls box robot]} moves] (parse-input input)
        robot-init (first robot)
        ]
    (->>
      (loop [moves moves
             boxes box
             robot robot-init]
        (if (empty? moves) boxes
          (let [[m & moves] moves
                [robot boxes] (one-step robot m boxes walls)]
    (recur moves boxes robot)
    )))
      (map (fn [[x y]] (+ x (* 100 y))))
      (reduce +))))




(defn parse-input-p2
  [input]
  (let [[warehouse moves] (cljstr/split input #"\n\n")
        warehouse (->> warehouse
                       cljstr/split-lines
                       (map
                         (fn [l]
                          (mapcat (fn[c]
                                    (case c
                                   \# [\# \#]
                                   \. [\. \.]
                                   \O [\[ \]]
                                   \@ [\@ \.])) l)))
                       (map-indexed (fn[y l]
                                      (apply merge-with into
                                      (map-indexed
                                        (fn [x c] (case c
                                                    \# {:walls #{[x y]}}
                                                    \[ {:box-l #{[x y]}}
                                                    \] {:box-r #{[x y]}}
                                                    \@ {:robot #{[x y]}}
                                                    {}))
                                        l))))
                       (apply merge-with into))
        moves (-> moves
                  (cljstr/replace "\n" ""))]
  [warehouse moves]))


(defn move-right
  [[rx ry] box-l box-r walls]
  (loop [testx (inc rx)
         mov-box-l #{}
         mov-box-r #{}
         ]
    (cond
      (contains? box-l [testx ry]) (recur (+ 2 testx) (cljset/union mov-box-l #{[testx ry]}) (cljset/union mov-box-r #{[(inc testx) ry]}))
      (contains? walls [testx ry]) [[rx ry] box-l box-r]
      :else [[(inc rx) ry]
             (cljset/union (cljset/difference box-l mov-box-l) (set (map (fn[[x y]] [(inc x) y]) mov-box-l)))
             (cljset/union (cljset/difference box-r mov-box-r) (set (map (fn[[x y]] [(inc x) y]) mov-box-r)))])))

(defn move-left
  [[rx ry] box-l box-r walls]
  (loop [testx (dec rx)
         mov-box-l #{}
         mov-box-r #{}
         ]
    (cond
      (contains? box-r [testx ry]) (recur (- testx 2) (cljset/union mov-box-l #{[(dec testx) ry]}) (cljset/union mov-box-r #{[testx ry]}))
      (contains? walls [testx ry]) [[rx ry] box-l box-r]
      :else [[(dec rx) ry]
             (cljset/union (cljset/difference box-l mov-box-l) (set (map (fn[[x y]] [(dec x) y]) mov-box-l)))
             (cljset/union (cljset/difference box-r mov-box-r) (set (map (fn[[x y]] [(dec x) y]) mov-box-r)))])))

(defn move-down
  [[rx ry] box-l box-r walls]
  (let [all-boxes (cljset/union box-l box-r)]
    (loop [testpos #{[rx (inc ry)]}
           mov-box-l #{}
           mov-box-r #{}
           ]
      (cond
        (some (fn [p] (contains? walls p)) testpos) [[rx ry] box-l box-r]
        (some (fn [p] (contains? all-boxes p)) testpos)
        (let [n-mov-box-l (cljset/intersection testpos box-l)
              n-mov-box-r (cljset/intersection testpos box-r)
              n-mov-box-l (cljset/union n-mov-box-l (set (map (fn [[x y]] [(dec x) y]) n-mov-box-r)))
              n-mov-box-r (cljset/union n-mov-box-r (set (map (fn [[x y]] [(inc x) y]) n-mov-box-l))) ]
          (recur (->> (cljset/union n-mov-box-l n-mov-box-r)
                      (map (fn [[x y]] [x (inc y)]))
                      set)
                 (cljset/union mov-box-l n-mov-box-l)
                 (cljset/union mov-box-r n-mov-box-r)))
        :else [[rx (inc ry)]
               (cljset/union (cljset/difference box-l mov-box-l) (set (map (fn[[x y]] [x (inc y)]) mov-box-l)))
               (cljset/union (cljset/difference box-r mov-box-r) (set (map (fn[[x y]] [x (inc y)]) mov-box-r)))]))
    ))

(defn move-up
  [[rx ry] box-l box-r walls]
  (let [all-boxes (cljset/union box-l box-r)]
    (loop [testpos #{[rx (dec ry)]}
           mov-box-l #{}
           mov-box-r #{}
           ]
      (cond
        (some (fn [p] (contains? walls p)) testpos) [[rx ry] box-l box-r]
        (some (fn [p] (contains? all-boxes p)) testpos)
        (let [n-mov-box-l (cljset/intersection testpos box-l)
              n-mov-box-r (cljset/intersection testpos box-r)
              n-mov-box-l (cljset/union n-mov-box-l (set (map (fn [[x y]] [(dec x) y]) n-mov-box-r)))
              n-mov-box-r (cljset/union n-mov-box-r (set (map (fn [[x y]] [(inc x) y]) n-mov-box-l))) ]
          (recur (->> (cljset/union n-mov-box-l n-mov-box-r)
                      (map (fn [[x y]] [x (dec y)]))
                      set)
                 (cljset/union mov-box-l n-mov-box-l)
                 (cljset/union mov-box-r n-mov-box-r)))

        :else [[rx (dec ry)]
               (cljset/union (cljset/difference box-l mov-box-l) (set (map (fn[[x y]] [x (dec y)]) mov-box-l)))
               (cljset/union (cljset/difference box-r mov-box-r) (set (map (fn[[x y]] [x (dec y)]) mov-box-r)))]))
    ))


(defn one-step-p2
  [robot symb box-l box-r walls]
  (let [dir (symb-to-dir symb)
        n-robot (map + robot dir)]
    ;; try to move the robot
    (cond
      (contains? walls n-robot) [robot box-l box-r]
      (contains? (cljset/union box-l box-r) n-robot) (case symb
                                                       \> (move-right robot box-l box-r walls)
                                                       \< (move-left  robot box-l box-r walls)
                                                       \v (move-down  robot box-l box-r walls)
                                                       \^ (move-up    robot box-l box-r walls))
      :else [n-robot box-l box-r])))


(defn d15p2
  [input]
  (let [[{:keys [robot walls box-l box-r]} moves] (parse-input-p2 input)
        robot (first robot)]
    (->>
      (loop [moves moves
             box-l box-l
             box-r box-r
             robot robot]
        (if (empty? moves) box-l
          (let [[m & moves] moves
;;                _ (prn [:robot robot :move m :box-l box-l :box-r box-r])
                [robot box-l box-r] (one-step-p2 robot m box-l box-r walls)]
            (recur moves box-l box-r robot))))
      (map (fn [[x y]] (+ x (* 100 y))))
      (reduce +)
        )))

(defn -main
  [& args]
  (println "day15")
  (println sample1)
  (newline)

  (comment
  (println "part1")
  (prn (d15p1 sample1))
  (prn [:solution1 sol1])
  (prn (d15p1 sample2))
  (prn [:solution2 sol2])
  (prn (d15p1 (slurp "input/day15.txt")))
)
  (newline)
  (println "part2")
  (prn (d15p2 sample3))
  (prn [:solution3 sol3p2])
  (prn (d15p2 sample2))
  (prn [:solution2 sol2p2])
  (prn (d15p2 (slurp "input/day15.txt")))
  )


(ns t.d20
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)
        ymax (count lines)
        xmax (count (first lines))
        all-map (->> lines
                     (map-indexed
                       (fn [y l]
                         (apply merge-with into
                                (map-indexed
                                  (fn [x c]
                                    (case c
                                      \S {:start [[x y]]}
                                      \# {:walls [[x y]]}
                                      \E {:end   [[x y]]}
                                      {}))
                                  l))))
                     (apply merge-with into)) ]
    {:walls (set (:walls all-map))
     :S (first (:start all-map))
     :E (first (:end all-map))
     :xmax xmax
     :ymax ymax}
  ))


(defn inside?
  [x y xmax ymax]
  (and (< -1 x xmax) (< -1 y ymax)))

(defn def-find-shortest-path
  [walls E xmax ymax]
  (fn [S]
    (loop [p [S]
           N 0]
        (let [np (mapcat
                   (fn [[x y]] [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])
                   p)
              np (->> np
                      (filter (fn [[x y]] (inside? x y xmax ymax)))
                      (filter #(not (contains? walls %))))]
          (if (some #(= % E) np) (inc N)
            (recur np (inc N))
            )))))

(defn def-shortest-path-with-known
  [walls E xmax ymax]
  (fn [S known]
    (loop [p [[0 S]]]
        (let [[[n [x y]] & p] p]
          (if (= [x y] E) n
            (let [np (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
                          (filter (fn [[x y]] (inside? x y xmax ymax)))
                          (filter #(not (contains? walls %)))
                          (map (fn [pp] [(inc n) pp]))
                          (map (fn [[np pp]] (if-let [npp (get known pp)] [(+ np npp) E] [np pp]))))]
            (recur (sort-by first (concat p np)))
            ))))))

(defn around
  [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn all-sh-paths
  [E walls xmax ymax]
  (let [test-point (fn [[x y]] (and (inside? x y xmax ymax) (not (contains? walls [x y]))))]
    (loop [pos (filter test-point (around E))
           known {E 0}
           dist 1]
      (if (empty? pos) known
        (let [npos (->> pos
                        (mapcat around)
                        (filter test-point)
                        (filter (fn [p] (not (contains? known p)))))
              nknown (->> pos
                          (map (fn [p] {p dist}))
                          (into {}))]
          (recur npos (merge known nknown) (inc dist))
          )))))
(defn d20p1-sample
  [input]
  (let [{:keys [walls S E xmax ymax]} (parse-input input)
        find-sh-path (def-shortest-path-with-known walls E xmax ymax)
        known {}
        toE (all-sh-paths E walls xmax ymax)
        fromS (all-sh-paths S walls xmax ymax)
        sh-path (get toE S)
        test-point (fn [[x y]] (and (inside? x y xmax ymax) (not (contains? walls [x y]))))
        ]
    (prn sh-path)
    (frequencies
      (for [[wx wy] walls
            j [[[(dec wx) wy] [(inc wx) wy]]
               [[(inc wx) wy] [(dec wx) wy]]
               [[wx (dec wy)] [wx (inc wy)]]
               [[wx (inc wy)] [wx (dec wy)]]]
            :when (every? test-point j)
            :when (< (+ (get fromS (first j)) (get toE (second j)) 2) sh-path)]
        (- sh-path (+ (get fromS (first j)) (get toE (second j)) 2) )))))

(defn d20p1
  [input]
  (let [{:keys [walls S E xmax ymax]} (parse-input input)
        find-sh-path (def-shortest-path-with-known walls E xmax ymax)
        known {}
        toE (all-sh-paths E walls xmax ymax)
        fromS (all-sh-paths S walls xmax ymax)
        sh-path (get toE S)
        test-point (fn [[x y]] (and (inside? x y xmax ymax) (not (contains? walls [x y]))))
        ]
    (prn sh-path)
    (apply +
           (for [[wx wy] walls
                 j [[[(dec wx) wy] [(inc wx) wy]]
                    [[(inc wx) wy] [(dec wx) wy]]
                    [[wx (dec wy)] [wx (inc wy)]]
                    [[wx (inc wy)] [wx (dec wy)]]]
                 :when (every? test-point j)
                 :when (<= 100 (- sh-path (+ (get fromS (first j)) (get toE (second j)) 2)))]
             1
             ))))

(defn d20p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day20")
  (println sample)
  (newline)

  (println "part1")
  (prn (d20p1-sample sample))
  (prn (d20p1 (slurp "input/day20.txt")))
  ;; 1438
  ;; That's not the right answer; your answer is too high.
  ;; Curiously, it's the right answer for someone else; you might be logged in to the wrong account or just unlucky.

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d20p2 sample))
  ;;  (prn (d20p2 (slurp "input/day20.txt")))
  )


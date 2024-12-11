(ns t.d11
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]
    [clojure.core.match :refer [match]]))

(def sample
"125 17")

(defn parse-input
  [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
  ))

(defn split-stone-2
  [stone]
  (let [str-stone (str stone)
        mid-i (quot (count str-stone) 2)]
    (->>
      [(take mid-i str-stone) (drop mid-i str-stone)]
      (map (fn [l] (apply str l)))
      (map parse-long))))

(defn evolve-stone
  [stone]
  (cond
   (= stone 0) [1]
   (even? (count (str stone))) (split-stone-2 stone)
   :else [(* stone 2024)]))

(def memo-evolve-stone (memoize evolve-stone))

(defn d11p1
  [input]
  (let [x (parse-input input)
        stones-start (map (fn [s] [s 0]) x)
        ]
    (loop [stones stones-start
           count-final-stones 0]
      (if (empty? stones) count-final-stones
        (let [[[stone n-blinks] & stones] stones
              n-stones (memo-evolve-stone stone)]
          (if (= 25 (inc n-blinks))
            (recur  stones (+ count-final-stones (count n-stones)))
            (recur (concat (map (fn [s] [s (inc n-blinks)]) n-stones) stones) count-final-stones)))))))

(defn d11p2
  [input]
  (let [x (parse-input input)
        stones-start (->> x
                          (map (fn [s] {s 1}))
                          (reduce (fn [n el] (merge-with + n el)))
                          )
        ]
    (loop [stones stones-start
           count-blinks 0]
      (if (= count-blinks 75) (->> stones (map second) (reduce +))
        (let [n-stones (->>
                         stones
                         (mapcat (fn [[st Nst]] (map (fn [s] {s Nst}) (memo-evolve-stone st))))
                         (reduce (fn [n el] (merge-with + n el))))]
          (recur n-stones (inc count-blinks)))))))



(defn -main
  [& args]
  (println "day11")
  (println sample)
  (newline)

;;  (println "part1")
;;  (prn (d11p1 sample))
;;  (prn (d11p1 (slurp "input/day11.txt")))
;; 33723062 is too hight - no memoization :
;; lein run -m t.d11  148.76s user 0.36s system 100% cpu 2:28.36 total
;; it was a problem with parsing the input
;; new result : 197357 with memoization = too rapid to show stats

  (newline)
  (println "part2")
  (prn (d11p2 sample))
  (prn (d11p2 (slurp "input/day11.txt")))
  )


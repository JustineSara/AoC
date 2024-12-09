(ns t.d9
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"2333133121414131402")

(defn parse-input
  [input]
  (map (comp parse-long str) input)
  )


(defn space
  [input]
  (loop [i (first input)
         r (rest input)
         free-space []
         occupied-space []
         id-occupied 0
         pos 0
         occupied? true]
    (if (nil? i)
      {:free free-space
       :occupied occupied-space}
      (let [n-pos (+ pos i)
            n-free-space (if occupied?
                           free-space
                           (conj free-space {:pos-min pos :pos-max (dec n-pos) :size i}))
            n-occupied-space (if occupied?
                               (conj occupied-space {:file-id id-occupied :pos-min pos :pos-max (dec n-pos)})
                               occupied-space)
            n-id-occupied (if occupied?
                            (inc id-occupied)
                            id-occupied)]
        (recur (first r) (rest r) n-free-space n-occupied-space n-id-occupied n-pos (not occupied?))))))

(defn d9p1
  [input]
  (let [x (parse-input input)
        ]
    (space x)
    ))

(defn d9p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day9")
  (println sample)
  (newline)

  (println "part1")
  (prn (d9p1 sample))
  ;;  (prn (d9p1 (slurp "input/day9.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d9p2 sample))
  ;;  (prn (d9p2 (slurp "input/day9.txt")))
  )


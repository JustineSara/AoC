(ns t.d9
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.core.match :refer [match]]
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
                               (conj occupied-space {:file-id id-occupied :pos-min pos :pos-max (dec n-pos) :file-size i})
                               occupied-space)
            n-id-occupied (if occupied?
                            (inc id-occupied)
                            id-occupied)]
        (recur (first r) (rest r) n-free-space n-occupied-space n-id-occupied n-pos (not occupied?))))))

(defn checksum
  [files]
  (->> files
       (map (fn [{:keys [pos-min pos-max file-id]}]
              (reduce + 0 (map (fn [pos] (* pos file-id)) (range pos-min (inc pos-max))))))
       (reduce + 0)))

(defn d9p1
  [input]
  (let [x (parse-input input)
        {:keys [free occupied]} (space x)
        free (sort-by :pos-min free)
        occupied (sort-by :pos-min > occupied)]
    (loop [f-space free
           o-space occupied]
      (if (empty? f-space)
        (checksum o-space)
        (let [to-be-filled (first f-space)
              to-be-moved (first o-space)
              size-diff (- (:size to-be-filled) (:file-size to-be-moved))]
          (prn [:free to-be-filled :occ to-be-moved])
          (cond
            ;; case 0 : the position of the file is before the free space
            (< (:pos-min to-be-moved) (:pos-min to-be-filled))
            (checksum o-space)
            ;; case 1 : same size
            (zero? size-diff)
            (let [moved-file {:file-id (:file-id to-be-moved)
                              :pos-min (:pos-min to-be-filled)
                              :pos-max (:pos-max to-be-filled)
                              :file-size (:file-size to-be-moved)}]
              (prn [:same-size :new-file moved-file])
              (recur (rest f-space) (sort-by :pos-min > (conj (rest o-space) moved-file))))
            ;; case 2 : more free space
            (pos? size-diff)
            (let [new-free-min (+ (:pos-min to-be-filled) (:file-size to-be-moved))
                  moved-file {:file-id (:file-id to-be-moved)
                              :pos-min (:pos-min to-be-filled)
                              :pos-max (dec new-free-min)
                              :file-size (:file-size to-be-moved)}
                  left-over-free-space {:pos-min new-free-min
                                        :pos-max (:pos-max to-be-filled)
                                        :size (- (:size to-be-filled) (:file-size to-be-moved))}]
              (prn [:more-free-space :new-free-space left-over-free-space :new-file moved-file])
              (recur (sort-by :pos-min (conj (rest f-space) left-over-free-space)) (sort-by :pos-min > (conj (rest o-space) moved-file))))
            ;; case 3 : file larger than free space
            (neg? size-diff)
            (let [new-file-max (- (:pos-max to-be-moved) (:size to-be-filled))
                  moved-file {:file-id (:file-id to-be-moved)
                              :pos-min (:pos-min to-be-filled)
                              :pos-max (:pos-max to-be-filled)
                              :file-size (:size to-be-filled)}
                  left-over-file (-> to-be-moved
                                     (assoc :pos-max new-file-max)
                                     (update :file-size - (:size to-be-filled))) ]
              (prn [:file-bigger :new-file-moved moved-file :left-over-file left-over-file])
              (recur (rest f-space) (sort-by :pos-min > (conj (rest o-space) moved-file left-over-file))))
            :else (prn ["case not done?!" :size-diff size-diff :to-be-moved to-be-moved :to-de-filled to-be-filled])
            )

          )
        )
      )
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


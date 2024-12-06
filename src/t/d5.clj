(ns t.d5
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))

(def sample
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse-input
  [input]
  (-> input
      (cljstr/split #"\n\n")
      ((fn [[r u]] {
                    :rules
                    (->> r
                         (cljstr/split-lines)
                         (map (fn [rule] (cljstr/split rule #"\|"))))
                    :updates
                    (->> u
                         (cljstr/split-lines)
                         (map (fn [upd] (cljstr/split upd #","))))
                    }))))

(defn valid-update?
  [rules upd]
  (let [
        filtered-rules (->> rules
                            (filter (fn [r]
                                      (some (fn [upd-num] (= upd-num (first r)))
                                            upd)))
                            (filter (fn [r]
                                      (some (fn [upd-num] (= upd-num (second r)))
                                            upd)))
                            )]
    (loop [up-page (first upd)
           up-rests (rest upd)
           rules-tbt filtered-rules]
      (if (nil? up-page)
        true
        (let [new-rules-tbt (filter (fn [r] (not= (first r) up-page)) rules-tbt)
              not-met-rules (filter (fn [r] (=   (second r) up-page)) rules-tbt)]
          (cond
            (> (count not-met-rules) 0) false
            (empty? new-rules-tbt) true
            :else (recur (first up-rests) (rest up-rests) new-rules-tbt)))))))

(defn select-mid-num
  [upd]
  (let [N (count upd)
        mid (/ (inc N) 2)]
    (nth upd (dec mid))))

(defn d5p1
  [input]
  (let [{:keys [rules updates]} (parse-input input)]
    (->>
      updates
      (filter (fn [upd] (valid-update? rules upd)))
      (map select-mid-num)
      (map parse-long)
      (apply +))))

(defn d5p2
  [input]
  (let [x (parse-input input)
        ]
    x
    ))

(defn -main
  [& args]
  (println "day5")
  (println sample)
  (newline)

  (println "part1")
  (prn (d5p1 sample))
  (prn (d5p1 (slurp "input/day5.txt")))

  ;;  (newline)
  ;;  (println "part2")
  ;;  (prn (d5p2 sample))
  ;;  (prn (d5p2 (slurp "input/day5.txt")))
  )


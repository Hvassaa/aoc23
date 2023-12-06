(ns aoc23.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn for-each-line
  [file-name fn]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file-name))]
    (doseq [line (line-seq rdr)]
      (fn line))))

(defn get-lines
  [file-name]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file-name))]
    (into [] (line-seq rdr))))

(defn get-day
  [day]
  (get-lines (str "day" day ".txt")))

(defn get-day-small
  [day]
  (get-day (str day "-small")))

(defn char-int
  "cast char to int"
  [c]
  (Character/digit c 10))

(defn char-is-int
  "check if char is a number (other than -1)"
  [c]
  (not= (char-int c) -1))

;; day 1
;; part 1

(defn find-first-number
  [line]
  (loop [c (first line)
         remain (rest line)]
    (if (char-is-int c)
      c
      (recur (first remain) (rest remain)))))

(defn line-to-number
  [line]
  (Integer/parseInt
   (str (find-first-number line) (find-first-number (reverse line)))))

(defn day1-1
  ([] (day1-1 (get-day 1)))
  ([lines] (reduce + (map line-to-number lines))))

;; part 2

(def spelled [{:spelled "one" :numerical 1}
              {:spelled "two" :numerical 2}
              {:spelled "three" :numerical 3}
              {:spelled "four" :numerical 4}
              {:spelled "five" :numerical 5}
              {:spelled "six" :numerical 6}
              {:spelled "seven" :numerical 7}
              {:spelled "eight" :numerical 8}
              {:spelled "nine" :numerical 9}])

(defn get-numerical-vals
  ([line]
   (loop [result []
          idx 0
          line line]
     (if (empty? line)
       result
       (let [c (first line)]
         (if (char-is-int c)
           (recur
            (conj result {:idx idx :numerical (char-int c)})
            (inc idx)
            (rest line))
           (recur result (inc idx) (rest line))))))))

(defn get-spelled-vals
  ([line]
   (flatten (map #(get-spelled-vals line %) spelled)))
  ([line spell]
   (loop [line line
          result []
          idx 0]
     (let [spelled (:spelled spell)
           numerical (:numerical spell)
           size (.length spelled)
           line-length (.length line)
           slice (if (>= line-length size) (subs line 0 size) "!")
           match (= slice spelled)]
       (if (empty? line)
         result
         (if match
           (recur (subs line 1) (conj result {:numerical numerical :idx idx}) (inc idx))
           (recur (subs line 1) result (inc idx))))))))

(defn get-unified
  [line]
  (concat (get-numerical-vals line) (get-spelled-vals line)))

(defn compose-num
  [line]
  (let [uni (get-unified line)]
    (Integer/parseInt (str
                       (:numerical (apply min-key :idx uni))
                       (:numerical (apply max-key :idx uni))))))

(defn day1-2
  ([] (day1-2 (get-day 1)))
  ([lines]
   (reduce + (map compose-num lines))))

;; day 2
;; part 1

(def amount {:red 12 :green 13 :blue 14})

(defn trim-game-num
  [line]
  (second (str/split line #": ")))

(defn get-games
  [line]
  (str/split (trim-game-num line) #"; "))

(defn split-game
  [game]
  (str/split game #", "))

(defn split-entry
  [entry]
  (let [splitted (str/split entry #" ")
        amount (Integer/parseInt (first splitted))
        color (keyword (second splitted))]
    {color amount}))

(defn color-key
  [m]
  (first (map key m)))

(defn get-max-amounts
  [line]
  (let [games (get-games line)]
    (reduce
     (fn [acc cur]
       (let [cur (split-entry cur)
             cur-key (color-key cur)
             cur-amount (cur-key cur)
             acc-amount (cur-key acc)]
         (if (> cur-amount acc-amount)
           (conj acc cur)
           acc)))
     {:red 0 :green 0 :blue 0}
     (flatten (map split-game games)))))

(defn get-possiblities
  [lines]
  (map (fn [game-amount]
         (and
          (<= (:red game-amount) (:red amount))
          (<= (:green game-amount) (:green amount))
          (<= (:blue game-amount) (:blue amount))))
   (map get-max-amounts lines)))

(defn day2-1
  ([] (day2-1 (get-day 2)))
  ([lines]
   (loop [result 0
          game 1
          remaining (get-possiblities lines)]
     (if (empty? remaining)
       result
       (if (first remaining)
         (recur (+ result game) (inc game) (rest remaining))
         (recur result (inc game) (rest remaining)))))))

(defn calc-power
  [maxes]
  (reduce * (map second maxes)))

(defn day2-2
  ([] (day2-2 (get-day 2)))
  ([lines]
   (loop [result 0
          remaining lines]
     (if (empty? remaining)
       result
       (let [maxes (get-max-amounts (first remaining))]
         (recur
          (+ result (calc-power maxes))
          (rest remaining)))))))

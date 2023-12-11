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
  (second (str/split line #": +")))

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

;; part 2

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

;; day 3
;; part 1

(defn get-nums-indexed
  [line]
  (filter #(char-is-int (first %)) (map vector line (range (.length line)))))

(defn get-takes
  ([line]
   (let [indexed (get-nums-indexed line)]
     (loop [result []
            tak 1
            prev (first indexed)
            cur (second indexed)
            tail (rest (rest indexed))]
       (if cur
         (if (= (second prev) (dec (second cur)))
           (recur result (inc tak) cur (first tail) (rest tail))
           (recur (conj result tak) 1 cur (first tail) (rest tail)))
         result)))))

(defn collect-group
  ([line] (collect-group (get-nums-indexed line) (get-takes line)))
  ([indexed takes]
   (loop [result []
          idx-tail indexed
          takes-head (first takes)
          takes-tail (rest takes)]
     (if takes-head
       (recur
        (conj result (take takes-head idx-tail))
        (drop takes-head idx-tail)
        (first takes-tail)
        (rest takes-tail))
       (conj result idx-tail)))))

(defn get-vals-with-poss
  [line]
  (let [a (collect-group line)]
    (map (fn [group]
           {:pos (map second group)
            :val (Integer/parseInt
                  (reduce str "" (map first group)))}) a)))

(defn check-left
  [l pos]
  (let [new-pos (dec pos)]
    (if (>= new-pos 0)
      (let [c (.charAt l new-pos)]
        (and (not (char-is-int c)) (not= \. c)))
      false)))

(defn check-right
  [l pos]
  (let [new-pos (inc pos)
        width (.length l)]
    (if (< new-pos width)
      (let [c (.charAt l new-pos)]
        (and (not (char-is-int c)) (not= \. c)))
      false)))

(defn check-under
  [l pos line-width]
  (let [width (.length l)
        new-pos (+ line-width pos)]
    (if (< new-pos width)
      (let [c (.charAt l new-pos)]
        (and (not (char-is-int c)) (not= \. c)))
      false)))

(defn check-over
  [l pos line-width]
  (let [width (.length l)
        new-pos (- pos line-width)]
    (if (>= new-pos 0)
      (let [c (.charAt l new-pos)]
        (and (not (char-is-int c)) (not= \. c)))
      false)))

(defn check-diagonals
  [l pos line-width]
  (or
   (check-under l (inc pos) line-width)
   (check-under l (dec pos) line-width)
   (check-over l (inc pos) line-width) 
   (check-over l (dec pos) line-width)))

(defn check-all
  [l pos line-width]
  (or
   (check-diagonals l pos line-width)
   (check-over l pos line-width)
   (check-under l pos line-width)
   (check-left l pos)
   (check-right l pos)))

(defn day3-1
  [lines]
  (let [width (.length (first lines))
        l (reduce str lines)
        vals-pos (get-vals-with-poss l)]
    (reduce + (map (fn [e]
                     (let [positions (:pos e)
                           value (:val e)
                           found (reduce #(or %1 %2) (map #(check-all l % width) positions))]
                       (if found
                         value
                         0)))
                   vals-pos))))

;; part 2

(defn check
  [c]
  (= \* c))

(defn check-inside
  [pos l]
  (and (>= pos 0) (< (.length l))))

(defn reach
  [position width]
  [(+ position 1)
   (- position 1)
   (+ position width)
   (- position width)
   (+ position width 1)
   (- (+ position width) 1)
   (+ (- position width) 1)
   (- position width 1)])

(defn m-reach
  [m width]
  (let [{pos :pos
         val :val} m
        new-pos (map #(reach % width) pos)
        c-new-pos (reduce into new-pos)]
    {:pos (set c-new-pos) :val val}))

(defn m-reach-all
  [lines]
  (let [l (reduce str lines)
        width (.length (first lines))
        vwp (get-vals-with-poss l)]
    (map #(m-reach % width) vwp)))

(defn get-star-pos
  [lines]
  (let [l (reduce str lines)]
    (map second
         (filter #(check (first %))
                 (map vector l (range (.length l)))))))

(defn star-matches
  [star-pos reaches]
  (filter (fn [{pos :pos
                val :val}]
            (contains? pos star-pos)) reaches))

(defn star-match-to-sum
  [m]
  (if (= (count m) 2)
    (reduce * (map :val m))
    0))

(defn star-sum
  [star-pos reaches]
  (star-match-to-sum (star-matches star-pos reaches)))

(defn day3-2
  ([] (day3-2 (get-day 3)))
  ([lines]
   (let [star-pos (get-star-pos lines)
         r (m-reach-all lines)]
     (reduce + (map (fn [star-pos]
                      (star-sum star-pos r)) star-pos)))))

;; day 4
;; part 1

(defn str-of-num-to-set
  [s]
  (set (map #(Integer/parseInt %) (str/split s  #" +"))))

(defn line-to-intersection
  [line]
  (let [halfs (str/split (trim-game-num line) #" \| +")]
    (clojure.set/intersection
     (str-of-num-to-set (first halfs))
     (str-of-num-to-set (second halfs)))))

(defn intersection-sum
  [i]
  (reduce (fn [acc cur]
            (if (= acc 0)
              1
              (* 2 acc))) 0 i))

(defn line-to-sum
  [line]
  (intersection-sum (line-to-intersection line)))

(defn day4-1
  ([] (day4-1 (get-day 4)))
  ([lines]
   (reduce + (map line-to-sum lines))))

;; part 2

(defn line-to-wins
  [line]
  (count (line-to-intersection line)))

(defn lines-to-wins
  [lines]
  (map line-to-wins lines))

(defn ones [n] (map (fn [a] 1) (range n)))

(defn zip-wins-with-1s
  [lines]
  (map vector (lines-to-wins lines) (ones (count lines))))

(defn inc-amount-tuple
  [i tup]
  (let [wins (first tup)
        amount (second tup)]
    [wins (+ i amount)]))

(defn do-win
  [n wins tups]
  (into (drop wins tups) (reverse (map #(inc-amount-tuple n %) (take wins tups)))))

(defn day4-2
  ([] (day4-2 (get-day 4)))
  ([lines]
   (let [w (zip-wins-with-1s lines)]
     (loop [result 0
            head (first w)
            tail (rest w)]
       (if head
         (let [wins (first head)
               amount (second head)
               new-tail (do-win amount wins tail)]
           (recur
            (+ result amount)
            (first new-tail)
            (rest new-tail)))
         result)))))

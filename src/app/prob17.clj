(ns app.prob17
  (:require [clojure.string :as s]))
;; https://projecteuler.net/problem=17

(def d2w
  {0 ""
   1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"})

(def up20
  (map d2w (range 0 20)))

(def up10
  (map d2w (range 0 10)))

(def up100
  (map d2w (map (partial * 10) (range 2 10))))

(def up1000
  (map #(str % (if (= % "") "" " hundred")) up10))

(defn join [r l s]
  (mapcat
   (fn [e]
     (map
      (fn [x]
        (str e
             (if (or (= e "") (= x "")) "" s)
             x))
      l))
   r))

(def in100 (join up100 up10 " "))

(def full100 (concat up20 in100))

(let [r (concat (drop 1 (join up1000 full100 " and ")) '("one thousand"))]
  {:verify (count r)
   :result (apply
            +
            (map
             (fn [s] (count (filter #(not= \space %) s)))
             r))})


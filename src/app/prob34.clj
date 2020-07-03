(ns app.prob34
  (:require [lib.numb :as numb]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=34

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

;; 1*1 + 1*2*3*4 + 1*2*3*4*5 = 1 + 1*2*3*4*(1 + 5) => 145

(def fact (memoize numb/fact))

(defn sum-fact [n]
  (->> (-> n str seq)
       (r/map #(- (int %) (int \0)))
       (r/map fact)
       (r/reduce +)))

;; (->>
;;  (range 0 10)
;;  (map #(vector % (fact %))))

;; => ([0 1] [1 1] [2 2] [3 6] [4 24] [5 120] [6 720] [7 5040] [8 40320] [9 362880])
;; if n contains 5 ~> n >= 120
;; if n contains 7 ~> n >= 5040
;; etc.
;; if n contains 9 ~> n >= 362880

(defn get-max-range [n]
  (if (> n (sum-fact n))
    n
    (recur (+ 9 (* n 10)))))

(time
 (->> (range 10 (get-max-range 9))
      (filter #(= % (sum-fact %)))))

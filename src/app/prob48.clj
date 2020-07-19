(ns app.prob48
  (:require
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=48
;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;; Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

(defn pow-tail [div x n]
  ;;pow-int from lib.numb causes intenger overflow due to squaring of the x, hence the simple loop
  (loop [z 1
         w n]
    (if (= w 0)
      z
      (recur (* x (mod z div))
             (dec w)))))

(defn sum [D N]
  (let [div (numb/pow-int 10 D)]
    (->> (range 1 (inc N))
         (map #(pow-tail div % %))
         (reduce +))))

(time
 (mod (sum 10 1000) (numb/pow-int 10 10)))

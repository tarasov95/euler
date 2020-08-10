(ns app.prob55
  (:require
   [clojure.test :as t]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=55

(defn Lychrel? [n]
  (loop [ix 1
         z (+ n (numb/backward n))]
    (cond (> ix 50) true
          (numb/palindrome? z) false
          :else (recur (inc ix) (+ z (numb/backward z))))))

(t/deftest Lychrel?-test
  (t/is (Lychrel? 196N))
  (t/is (Lychrel? 4994N))
  (t/is (not (Lychrel? 349))))

(defn solve []
  (->> (range 1N 10000N)
       (filter Lychrel?)
       (count)))

(time (solve))

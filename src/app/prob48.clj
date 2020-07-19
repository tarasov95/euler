(ns app.prob48
  (:require
   [lib.prime :as prime]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=48
;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;; Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

(defn pow-tail
  ([div x pw] (pow-tail div x pw 1))
  ([div x pw z]
   (cond (<= pw 1) (mod (* x z) div)
         (even? pw) (recur div (* x x) (quot pw 2) z)
         :else (recur div x (dec pw) (mod (* x z) div)))))

(defn sum [D N]
  (let [div (numb/pow-int 10 D)]
    (->> (range 1N (inc N))
         (map #(pow-tail div % %))
         (reduce +))))

(mod (sum 10 1000) (numb/pow-int 10 10))

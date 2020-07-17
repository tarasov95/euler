(ns app.prob46
  (:require
   [lib.prime :as prime]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=46
;; It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
;; 9 = 7 + 2×12
;; 15 = 7 + 2×22
;; 21 = 3 + 2×32
;; 25 = 7 + 2×32
;; 27 = 19 + 2×22
;; 33 = 31 + 2×12
;; It turns out that the conjecture was false.
;; What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

(defn gen-oddcomp [s e]
  (drop 1 (range s e 2)))

;;n  = p + 2x^2
;;x = sqrt (n-p/2)
(defn Goldbach [*pr* n]
  [n
   (->> (take-while (partial >= (- n 2)) *pr*)
        (filter #(even? (- n %)))
        (filter #(numb/natural? (Math/sqrt (/ (- n %) 2)))))])

(defn solve []
  (let [*pr* (prime/primes-all)
        pl   (drop 1 *pr*)
        pr   (drop 1 pl)]
    (->> (map gen-oddcomp pl pr)
         (filter (comp not empty?))
         (mapcat identity) ;;cause (reduce concat) is not lazy
         (map (partial Goldbach *pr*))
         (filter #(empty? (second %)))
         (take 1))))

(time (doall (solve)))

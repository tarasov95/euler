(ns app.prob69
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=69

;; n/f ~> n/(n-1) when n is a prime
;; p*(1/(1-1/p))
;; n/f ~> 1/P(1-1/p) for each p dividing n
;; ~> have to maximize 1/P(1-1/p) ~> min P(1-1/p) ~> as many components (all < 1) in P as possible with p as low as possible
;; ~> n would be the one with the largest number of prime factors

(defn P [n]
  (->> (prime/prime-fact n)
      (map :fac)
      (map #(- 1 (/ 1 %)))
      (reduce *)))

(defn phi [n]
  (* n (P n)))

;; (->> (range 2 1000)
;;      (map #(vector (/ (float %) (phi %)) % (map :fac (prime/prime-fact %))) )
;;      (sort-by first)
;;      (take-last 20))

(defn solve []
  (reduce
   (fn [z e]
     (if (> (* z e) 1000000)
       (reduced z)
       (* z e)))
   (prime/primes-all)))
